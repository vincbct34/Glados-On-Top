/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** ProcessManager - Handle actor model operations (Clean Architecture)
*/

#pragma once

#include "common/Types.hpp"
#include "common/ErrorCode.hpp"
#include "core/RuntimeValue.hpp"
#include "interfaces/IProcessManager.hpp"
#include "process/ProcessIdentity.hpp"
#include "process/ProcessMailbox.hpp"
#include "process/ProcessTemplate.hpp"
#include "process/Process.hpp"
#include "process/Message.hpp"
#include "vm/VariableStore.hpp"

#include <expected>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <queue>
#include <string>
#include <vector>

namespace rat
{
    // Forward declarations
    struct Instruction;

    /**
     * @class ProcessManager
     * @brief Manages processes and message passing with clean architecture
     * Implements interfaces::IProcessManager for dependency inversion
     *
     * Responsibilities:
     * - Process template management
     * - Process lifecycle (creation, termination)
     * - Message routing
     * - PID allocation
     *
     * @author Robin Toillon
     */
    class alignas(64) ProcessManager : public interfaces::IProcessManager {
        public:
            ProcessManager(void);
            ~ProcessManager() override = default;

            ProcessManager(const ProcessManager &) = delete;
            ProcessManager &operator=(const ProcessManager &) = delete;
            ProcessManager(ProcessManager &&) noexcept = default;
            ProcessManager &operator=(ProcessManager &&) noexcept = default;

            [[nodiscard]]
            auto defineProcess(const std::string &name,
                               const std::vector<std::string> &params,
                               const core::InstructionSequence &body)
                -> std::expected<void, VMError> override;

            [[nodiscard]]
            std::expected<rat::process::ProcessIdentity, VMError> spawn(
                const std::string &name,
                const std::vector<RuntimeValue> &args) override;

            [[nodiscard]]
            auto send(rat::process::ProcessIdentity pid, RuntimeValue message)
                -> std::expected<void, VMError> override;

            [[nodiscard]]
            auto receive(process::ProcessIdentity pid)
                -> std::expected<RuntimeValue, VMError> override;

            [[nodiscard]]
            auto terminate(rat::process::ProcessIdentity pid)
                -> std::expected<void, VMError> override;

            [[nodiscard]]
            rat::process::ProcessIdentity getCurrentPid(void) const noexcept override
            {
                return this->_currentPid;
            }

            void setCurrentPid(rat::process::ProcessIdentity pid) noexcept override
            {
                this->_currentPid = pid;
            }

            /**
             * @brief Get process template by name
             * @param name Template name
             * @return Optional template
             */
            [[nodiscard]]
            const ProcessTemplate *getTemplate(const std::string &name) const;

            /**
             * @brief Get process by PID
             * @param pid Process ID
             * @return Optional reference to process
             */
            [[nodiscard]]
            std::expected<Process *, VMError> getProcess(
                rat::process::ProcessIdentity pid);

            [[nodiscard]]
            std::expected<const Process *, VMError> getProcess(
                rat::process::ProcessIdentity pid) const;

            /**
             * @brief Clear all processes and templates
             */
            void clear(void) noexcept;

            [[nodiscard]]
            auto createInstance(const std::string &name,
                                const std::vector<RuntimeValue> &args)
                -> std::expected<int64_t, std::string>;

            [[nodiscard]]
            auto sendMessage(int64_t targetPid, int64_t senderPid,
                             RuntimeValue message)
                -> std::expected<void, std::string>;

            [[nodiscard]]
            auto sendMessage(int64_t targetPid, RuntimeValue message)
                -> std::expected<void, std::string>;

            [[nodiscard]]
            auto waitMessage(int64_t pid)
                -> std::expected<Message, std::string>;

            [[nodiscard]]
            auto exitProcess(int64_t pid)
                -> std::expected<void, std::string>;

        private:
            /**
             * @brief Allocate new unique PID
             * @return New PID
             */
            [[nodiscard]]
            rat::process::ProcessIdentity allocatePid(void) noexcept
            {
                return this->_nextPid++;
            }

            /**
             * @brief Validate process arguments
             * @param tmpl Process template
             * @param args Arguments
             * @return Result
             */
            [[nodiscard]]
            auto validateArguments(const ProcessTemplate &tmpl,
                                   const std::vector<RuntimeValue> &args) const
                -> std::expected<void, VMError>;

            /**
             * @brief Initialize process with arguments
             * @param process Process instance
             * @param tmpl Template
             * @param args Arguments
             * @return Result
             */
            [[nodiscard]]
            auto initializeProcess(Process &process,
                                       const ProcessTemplate &tmpl,
                                       const std::vector<RuntimeValue> &args)
                -> std::expected<void, VMError>;

            std::map<std::string, ProcessTemplate> _templates;
            std::map<rat::process::ProcessIdentity, Process> _processes;
            process::ProcessIdentity _nextPid;
            process::ProcessIdentity _currentPid;
            mutable std::mutex _mutex;
    };
}
