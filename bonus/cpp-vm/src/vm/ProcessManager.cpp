/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** ProcessManager Implementation (Clean Architecture)
*/

#include "ProcessManager.hpp"
#include <format>
#include <algorithm>

namespace rat
{
    ProcessManager::ProcessManager(void) : _nextPid(1), _currentPid(0)
    {
    }

    std::expected<void, VMError> ProcessManager::defineProcess(
        const std::string &name,
        const std::vector<std::string> &params,
        const std::vector<Instruction> &body)
    {
        std::lock_guard lock(_mutex);

        if (name.empty())
            return std::unexpected(VMError::InvalidOperand{"Process name cannot be empty"});

        if (this->_templates.contains(name))
            return std::unexpected(VMError::InvalidState{"Process already defined"});

        this->_templates.emplace(name, ProcessTemplate(name, params, body));
        return {};
    }

    std::expected<rat::process::ProcessIdentity, VMError> ProcessManager::spawn(
        const std::string &name,
        const std::vector<RuntimeValue> &args)
    {
        std::lock_guard lock(_mutex);

        auto it = this->_templates.find(name);
        if (it == this->_templates.end())
            return std::unexpected(VMError::FunctionNotFound{name});

        const auto &tmpl = it->second;

        if (auto result = validateArguments(tmpl, args); !result)
            return std::unexpected(result.error());

        rat::process::ProcessIdentity pid = allocatePid();
        auto [procIt, inserted] = this->_processes.try_emplace(pid, pid, name, tmpl);

        if (!inserted)
            return std::unexpected(VMError::InvalidState{"Failed to create process"});

        if (auto result = initializeProcess(procIt->second, tmpl, args); !result)
        {
            this->_processes.erase(procIt);
            return std::unexpected(result.error());
        }

        return pid;
    }

    std::expected<void, VMError> ProcessManager::send(rat::process::ProcessIdentity pid, RuntimeValue message)
    {
        std::lock_guard lock(_mutex);

        auto it = this->_processes.find(pid);
        if (it == this->_processes.end())
        {
            return std::unexpected(VMError::ProcessNotFound{pid.value()});
        }

        it->second.mailbox().enqueue(Message(_currentPid, std::move(message)));
        return {};
    }

    std::expected<RuntimeValue, VMError> ProcessManager::receive(rat::process::ProcessIdentity pid)
    {
        std::lock_guard lock(_mutex);

        auto it = this->_processes.find(pid);
        if (it == this->_processes.end())
        {
            return std::unexpected(VMError::ProcessNotFound{pid.value()});
        }

        auto msg = it->second.mailbox().dequeue();
        if (!msg)
        {
            return std::unexpected(VMError::MailboxEmpty{pid.value()});
        }

        return msg->takeContent();
    }

    std::expected<void, VMError> ProcessManager::terminate(rat::process::ProcessIdentity pid)
    {
        std::lock_guard lock(_mutex);

        auto it = this->_processes.find(pid);
        if (it == this->_processes.end())
        {
            return std::unexpected(VMError::ProcessNotFound{pid.value()});
        }

        this->_processes.erase(it);
        return {};
    }

    const ProcessTemplate *ProcessManager::getTemplate(const std::string &name) const
    {
        std::lock_guard lock(this->_mutex);

        auto it = _templates.find(name);
        if (it == _templates.end())
        {
            return nullptr;
        }
        return &it->second;
    }

    std::expected<Process *, VMError> ProcessManager::getProcess(rat::process::ProcessIdentity pid)
    {
        std::lock_guard lock(this->_mutex);

        auto it = _processes.find(pid);
        if (it == _processes.end())
        {
            return std::unexpected(VMError::ProcessNotFound{pid.value()});
        }
        return &it->second;
    }

    std::expected<const Process *, VMError> ProcessManager::getProcess(rat::process::ProcessIdentity pid) const
    {
        std::lock_guard lock(this->_mutex);

        auto it = this->_processes.find(pid);
        if (it == this->_processes.end())
        {
            return std::unexpected(VMError::ProcessNotFound{pid.value()});
        }
        return &it->second;
    }

    void ProcessManager::clear(void) noexcept
    {
        std::lock_guard lock(_mutex);
        this->_templates.clear();
        this->_processes.clear();
        this->_nextPid = rat::process::ProcessIdentity(1);
        this->_currentPid = rat::process::ProcessIdentity(0);
    }

    auto ProcessManager::validateArguments(const ProcessTemplate &tmpl,
                                           const std::vector<RuntimeValue> &args) const
        -> std::expected<void, VMError>
    {
        if (args.size() != tmpl.paramCount())
            return std::unexpected(VMError::InvalidArity{tmpl.name(), tmpl.paramCount(), args.size()});
        return {};
    }

    auto ProcessManager::initializeProcess(Process &process,
                                           const ProcessTemplate &tmpl,
                                           const std::vector<RuntimeValue> &args)
        -> std::expected<void, VMError>
    {
        const auto &params = tmpl.params();
        for (size_t i = 0; i < params.size(); ++i)
        {
            if (auto result = process.locals().set(params[i], args[i]); !result)
            {
                return std::unexpected(VMError::InvalidState{"Failed to bind argument to parameter"});
            }
        }
        return {};
    }

    // === Legacy API Compatibility ===

    // These methods maintain backward compatibility with existing code
    // while delegating to the new clean interface

    auto ProcessManager::createInstance(
        const std::string &name,
        const std::vector<RuntimeValue> &args)
        -> std::expected<int64_t, std::string>
    {
        auto result = spawn(name, args);
        if (!result)
        {
            return std::unexpected("Spawn failed"); // TODO: convert VMError to string
        }
        return result->value();
    }

    auto ProcessManager::sendMessage(int64_t targetPid, int64_t senderPid, RuntimeValue message)
        -> std::expected<void, std::string>
    {
        auto savedPid = _currentPid;
        _currentPid = rat::process::ProcessIdentity(senderPid);
        auto result = send(rat::process::ProcessIdentity(targetPid), std::move(message));
        _currentPid = savedPid;
        if (!result)
        {
            return std::unexpected("Send failed"); // TODO: convert VMError to string
        }
        return {};
    }

    auto ProcessManager::sendMessage(int64_t targetPid, RuntimeValue message)
        -> std::expected<void, std::string>
    {
        auto result = send(rat::process::ProcessIdentity(targetPid), std::move(message));
        if (!result)
        {
            return std::unexpected("Send failed");
        }
        return {};
    }

    auto ProcessManager::waitMessage(int64_t pid)
        -> std::expected<Message, std::string>
    {
        auto result = receive(rat::process::ProcessIdentity(pid));
        if (!result)
        {
            return std::unexpected("Receive failed");
        }
        return Message(_currentPid, std::move(*result));
    }

    auto ProcessManager::exitProcess(int64_t pid)
        -> std::expected<void, std::string>
    {
        auto result = terminate(rat::process::ProcessIdentity(pid));
        if (!result)
        {
            return std::unexpected("Terminate failed");
        }
        return {};
    }

}
