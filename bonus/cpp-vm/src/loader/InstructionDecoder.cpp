/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** InstructionDecoder Class Implementation - Modern C++23 with Jump Table
*/

#include "InstructionDecoder.hpp"
#include <cstring>
#include <expected>
#include <iterator>
#include <string>
#include <variant>
#include <vector>
#include <array>
#include <algorithm>

namespace rat
{
    // Constructor
    InstructionDecoder::InstructionDecoder(void)
    {
        initializeDecoderTable();
    }

    auto InstructionDecoder::decode(const std::vector<std::byte> &buffer)
        -> std::expected<std::vector<Instruction>, VMError>
    {
        if (buffer.size() < 6)
        {
            return std::unexpected(VMError::InvalidOperand{"Buffer too small for header"});
        }

        std::vector<Instruction> instructions;
        auto it = buffer.cbegin() + 6; // Skip 6-byte header
        auto end = buffer.cend();

        // Read instruction count with error handling
        auto instrCountRes = decodeVarInt(it, end);
        if (!instrCountRes)
        {
            return std::unexpected(instrCountRes.error());
        }

        const size_t instrCount = static_cast<size_t>(*instrCountRes);
        instructions.reserve(instrCount); // Pre-allocate for performance

        // Decode all instructions
        for (size_t i = 0; i < instrCount; ++i)
        {
            auto instrRes = decodeInstruction(it, end);
            if (!instrRes)
            {
                return std::unexpected(instrRes.error());
            }
            instructions.push_back(std::move(*instrRes));
        }

        return instructions;
    }

    // ========== Jump Table Initialization ==========
    void InstructionDecoder::initializeDecoderTable()
    {
        // Initialize all entries to nullptr for safety
        this->_decoderTable.fill(nullptr);

        this->_decoderTable[6] = &InstructionDecoder::decodeNoArgInstruction<Instruction::PushUnit>;
        this->_decoderTable[8] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Dup>;
        this->_decoderTable[13] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Index>;
        this->_decoderTable[14] = &InstructionDecoder::decodeNoArgInstruction<Instruction::ArrayLength>;
        this->_decoderTable[18] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Add>;
        this->_decoderTable[19] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Sub>;
        this->_decoderTable[20] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Mul>;
        this->_decoderTable[21] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Div>;
        this->_decoderTable[22] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Concat>;
        this->_decoderTable[27] = &InstructionDecoder::decodeNoArgInstruction<Instruction::CmpEq>;
        this->_decoderTable[28] = &InstructionDecoder::decodeNoArgInstruction<Instruction::CmpNeq>;
        this->_decoderTable[29] = &InstructionDecoder::decodeNoArgInstruction<Instruction::CmpLt>;
        this->_decoderTable[30] = &InstructionDecoder::decodeNoArgInstruction<Instruction::CmpLe>;
        this->_decoderTable[31] = &InstructionDecoder::decodeNoArgInstruction<Instruction::CmpGt>;
        this->_decoderTable[32] = &InstructionDecoder::decodeNoArgInstruction<Instruction::CmpGe>;
        this->_decoderTable[33] = &InstructionDecoder::decodeNoArgInstruction<Instruction::LogicAnd>;
        this->_decoderTable[34] = &InstructionDecoder::decodeNoArgInstruction<Instruction::LogicOr>;
        this->_decoderTable[35] = &InstructionDecoder::decodeNoArgInstruction<Instruction::LogicNot>;
        this->_decoderTable[36] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Negate>;
        this->_decoderTable[37] = &InstructionDecoder::decodeNoArgInstruction<Instruction::PushNone>;
        this->_decoderTable[40] = &InstructionDecoder::decodeNoArgInstruction<Instruction::PushJust>;
        this->_decoderTable[41] = &InstructionDecoder::decodeNoArgInstruction<Instruction::PushLeft>;
        this->_decoderTable[42] = &InstructionDecoder::decodeNoArgInstruction<Instruction::PushRight>;
        this->_decoderTable[47] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Send>;
        this->_decoderTable[48] = &InstructionDecoder::decodeNoArgInstruction<Instruction::WaitMessage>;
        this->_decoderTable[54] = &InstructionDecoder::decodeNoArgInstruction<Instruction::MatchWildcard>;
        this->_decoderTable[58] = &InstructionDecoder::decodeNoArgInstruction<Instruction::ProcessLoop>;
        this->_decoderTable[59] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Self>;
        this->_decoderTable[60] = &InstructionDecoder::decodeNoArgInstruction<Instruction::ExitProcess>;
        this->_decoderTable[69] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Return>;
        this->_decoderTable[70] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Print>;
        this->_decoderTable[71] = &InstructionDecoder::decodeNoArgInstruction<Instruction::Halt>;

        this->_decoderTable[2] = &InstructionDecoder::decodeStringInstruction<Instruction::PushString>;
        this->_decoderTable[3] = &InstructionDecoder::decodeStringInstruction<Instruction::PushAtom>;
        this->_decoderTable[9] = &InstructionDecoder::decodeStringInstruction<Instruction::LoadVar>;
        this->_decoderTable[10] = &InstructionDecoder::decodeStringInstruction<Instruction::StoreVar>;
        this->_decoderTable[11] = &InstructionDecoder::decodeStringInstruction<Instruction::LoadLocal>;
        this->_decoderTable[12] = &InstructionDecoder::decodeStringInstruction<Instruction::StoreLocal>;
        this->_decoderTable[15] = &InstructionDecoder::decodeStringInstruction<Instruction::InitState>;
        this->_decoderTable[16] = &InstructionDecoder::decodeStringInstruction<Instruction::GetState>;
        this->_decoderTable[17] = &InstructionDecoder::decodeStringInstruction<Instruction::SetState>;
        this->_decoderTable[23] = &InstructionDecoder::decodeStringInstruction<Instruction::IncVar>;
        this->_decoderTable[24] = &InstructionDecoder::decodeStringInstruction<Instruction::DecVar>;
        this->_decoderTable[25] = &InstructionDecoder::decodeStringInstruction<Instruction::IncVarPost>;
        this->_decoderTable[26] = &InstructionDecoder::decodeStringInstruction<Instruction::DecVarPost>;
        this->_decoderTable[39] = &InstructionDecoder::decodeStringInstruction<Instruction::GetField>;
        this->_decoderTable[43] = &InstructionDecoder::decodeStringInstruction<Instruction::MaybeBind>;
        this->_decoderTable[44] = &InstructionDecoder::decodeStringInstruction<Instruction::EitherBind>;
        this->_decoderTable[45] = &InstructionDecoder::decodeStringInstruction<Instruction::DefineProcess>;
        this->_decoderTable[46] = &InstructionDecoder::decodeStringInstruction<Instruction::CreateInstance>;
        this->_decoderTable[49] = &InstructionDecoder::decodeStringInstruction<Instruction::DefineFunction>;
        this->_decoderTable[50] = &InstructionDecoder::decodeStringInstruction<Instruction::CallFunction>;
        this->_decoderTable[51] = &InstructionDecoder::decodeStringInstruction<Instruction::MatchAtom>;
        this->_decoderTable[52] = &InstructionDecoder::decodeStringInstruction<Instruction::MatchVar>;
        this->_decoderTable[57] = &InstructionDecoder::decodeStringInstruction<Instruction::MatchString>;
        this->_decoderTable[61] = &InstructionDecoder::decodeStringInstruction<Instruction::StaticCast>;
        this->_decoderTable[62] = &InstructionDecoder::decodeStringInstruction<Instruction::ReinterpretCast>;
        this->_decoderTable[63] = &InstructionDecoder::decodeStringInstruction<Instruction::ConstCast>;
        this->_decoderTable[67] = &InstructionDecoder::decodeStringInstruction<Instruction::Label>;
        this->_decoderTable[68] = &InstructionDecoder::decodeStringInstruction<Instruction::Call>;

        this->_decoderTable[0] = &InstructionDecoder::decodeVarIntInstruction<Instruction::PushInt>;
        this->_decoderTable[4] = &InstructionDecoder::decodeVarIntInstruction<Instruction::PushTuple>;
        this->_decoderTable[5] = &InstructionDecoder::decodeVarIntInstruction<Instruction::PushArray>;
        this->_decoderTable[7] = &InstructionDecoder::decodeVarIntInstruction<Instruction::PopN>;
        this->_decoderTable[53] = &InstructionDecoder::decodeVarIntInstruction<Instruction::MatchTuple>;
        this->_decoderTable[55] = &InstructionDecoder::decodeVarIntInstruction<Instruction::MatchInt>;
        this->_decoderTable[64] = &InstructionDecoder::decodeVarIntInstruction<Instruction::Jump>;
        this->_decoderTable[65] = &InstructionDecoder::decodeVarIntInstruction<Instruction::JumpIfFalse>;
        this->_decoderTable[66] = &InstructionDecoder::decodeVarIntInstruction<Instruction::JumpIfTrue>;
    }

    auto InstructionDecoder::decodeInstruction(ByteIterator &it, ByteIterator end)
        -> std::expected<Instruction, VMError>
    {
        if (it == end)
        {
            return std::unexpected(VMError::InvalidOperand{"Unexpected end of bytecode"});
        }

        const uint8_t opcodeByte = static_cast<uint8_t>(*it);
        ++it;

        // Special cases that don't fit the template pattern
        if (opcodeByte == 1) { // PUSH_FLOAT
            if (std::distance(it, end) < static_cast<ptrdiff_t>(sizeof(double)))
                return std::unexpected(VMError::InvalidOperand{"Incomplete float"});
            double val;
            std::memcpy(&val, &*it, sizeof(double));
            std::advance(it, sizeof(double));
            return Instruction::PushFloat{val};
        }

        if (opcodeByte == 38) { // PUSH_BOOL
            if (std::distance(it, end) < static_cast<ptrdiff_t>(sizeof(bool)))
                return std::unexpected(VMError::InvalidOperand{"Incomplete bool"});
            const uint8_t b = static_cast<uint8_t>(*it);
            ++it;
            return Instruction::PushBool{static_cast<bool>(b)};
        }

        if (opcodeByte == 56)
        { // MATCH_BOOL
            if (std::distance(it, end) < static_cast<ptrdiff_t>(sizeof(bool)))
            {
                return std::unexpected(VMError::InvalidOperand{"Incomplete bool"});
            }
            const uint8_t b = static_cast<uint8_t>(*it);
            ++it;
            // Note: offset handling might be needed separately if this instruction uses it
            return Instruction{Instruction::MatchBool{static_cast<bool>(b)}};
        }

        // Use jump table for other instructions
        if (opcodeByte >= MAX_OPCODE || this->_decoderTable[opcodeByte] == nullptr)
        {
            return std::unexpected(VMError::InvalidOpcode{opcodeByte});
        }

        return (this->*_decoderTable[opcodeByte])(it, end);
    }

    template <typename InstrType>
    auto InstructionDecoder::decodeNoArgInstruction(ByteIterator &, ByteIterator)
        -> std::expected<Instruction, VMError>
    {
        return Instruction{InstrType{}};
    }

    template <typename InstrType>
    auto InstructionDecoder::decodeStringInstruction(ByteIterator &it, ByteIterator end)
        -> std::expected<Instruction, VMError>
    {
        auto str = decodeString(it, end);
        if (!str)
            return std::unexpected(str.error());

        if constexpr (std::is_same_v<InstrType, Instruction::MatchAtom> || std::is_same_v<InstrType, Instruction::MatchString>)
            return Instruction{InstrType{std::move(*str)}};
        else
            return Instruction{InstrType{std::move(*str)}};
    }

    template <typename InstrType>
    auto InstructionDecoder::decodeVarIntInstruction(ByteIterator &it, ByteIterator end)
        -> std::expected<Instruction, VMError>
    {
        auto val = decodeVarInt(it, end);
        if (!val)
            return std::unexpected(val.error());

        if constexpr (std::is_same_v<InstrType, Instruction::MatchTuple>) {
            return Instruction::MatchTuple{static_cast<int>(*val)};
        } else if constexpr (std::is_same_v<InstrType, Instruction::MatchInt>) {
            return Instruction::MatchInt{static_cast<int64_t>(*val)};
        } else
            return InstrType{static_cast<int>(*val)};
    }

    auto InstructionDecoder::decodeVarInt(ByteIterator &it, ByteIterator end)
        -> std::expected<int64_t, VMError>
    {
        if (it == end)
            return std::unexpected(VMError::InvalidOperand{"Unexpected end while decoding varint"});

        int64_t result = 0;
        int shift = 0;

        while (it != end) {
            const uint8_t byte = static_cast<uint8_t>(*it);
            ++it;

            result |= static_cast<int64_t>(byte & 0x7F) << shift;

            if (!(byte & 0x80))
                return result;

            shift += 7;
            if (shift >= 64)
                return std::unexpected(VMError::InvalidOperand{"VarInt overflow - too many bytes"});
        }

        return std::unexpected(VMError::InvalidOperand{"Incomplete varint - unexpected end"});
    }

    auto InstructionDecoder::decodeString(ByteIterator &it, ByteIterator end)
        -> std::expected<std::string, VMError>
    {
        auto lenRes = decodeVarInt(it, end);
        if (!lenRes)
            return std::unexpected(lenRes.error());

        const size_t len = static_cast<size_t>(*lenRes);
        const auto remaining = std::distance(it, end);

        if (remaining < static_cast<ptrdiff_t>(len))
            return std::unexpected(VMError::InvalidOperand{"Incomplete string data"});
        std::string str;
        str.reserve(len);
        str.assign(reinterpret_cast<const char *>(&*it), len);

        std::advance(it, len);
        return str;
    }

    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::PushUnit>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Dup>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Index>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::ArrayLength>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Add>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Sub>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Mul>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Div>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Concat>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::CmpEq>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::CmpNeq>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::CmpLt>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::CmpLe>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::CmpGt>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::CmpGe>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::LogicAnd>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::LogicOr>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::LogicNot>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Negate>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::PushNone>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::PushJust>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::PushLeft>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::PushRight>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Send>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::WaitMessage>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::MatchWildcard>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::ProcessLoop>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Self>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::ExitProcess>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Return>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Print>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeNoArgInstruction<Instruction::Halt>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;

    template auto InstructionDecoder::decodeStringInstruction<Instruction::PushString>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::PushAtom>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::LoadVar>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::StoreVar>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::LoadLocal>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::StoreLocal>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::InitState>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::GetState>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::SetState>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::IncVar>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::DecVar>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::IncVarPost>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::DecVarPost>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::GetField>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::MaybeBind>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::EitherBind>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::DefineProcess>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::CreateInstance>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::DefineFunction>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::CallFunction>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::MatchVar>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::StaticCast>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::ReinterpretCast>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::ConstCast>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::Label>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeStringInstruction<Instruction::Call>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;

    template auto InstructionDecoder::decodeVarIntInstruction<Instruction::PushInt>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeVarIntInstruction<Instruction::PushTuple>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeVarIntInstruction<Instruction::PushArray>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeVarIntInstruction<Instruction::PopN>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeVarIntInstruction<Instruction::Jump>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeVarIntInstruction<Instruction::JumpIfFalse>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
    template auto InstructionDecoder::decodeVarIntInstruction<Instruction::JumpIfTrue>(ByteIterator &, ByteIterator) -> std::expected<Instruction, VMError>;
}
