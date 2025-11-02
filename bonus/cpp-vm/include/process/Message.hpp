/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Message - Value object for inter-process communication
*/

#pragma once

#include "common/Types.hpp"
#include "core/RuntimeValue.hpp"

namespace rat
{
    /**
     * @class Message
     * @brief Immutable message for inter-process communication
     *
     * Value object that encapsulates a message sent between processes.
     * Contains sender information and the message content.
     * Follows value semantics for safety and clarity.
     *
     * @author Robin Toillon
     */
    class Message {
    public:
        /**
         * @brief Construct a message
         * @param senderPid ID of the sending process
         * @param content Message payload
         */
        Message(ProcessId senderPid, RuntimeValue content);

        // Value semantics - allow copy/move
        Message(const Message&) = default;
        Message& operator=(const Message&) = default;
        Message(Message&&) noexcept = default;
        Message& operator=(Message&&) noexcept = default;

        /**
         * @brief Get sender process ID
         * @return Sender PID
         */
        [[nodiscard]] ProcessId sender(void) const noexcept;

        /**
         * @brief Get message content (const reference)
         * @return Content reference
         */
        [[nodiscard]] const RuntimeValue& content(void) const noexcept;

        /**
         * @brief Take ownership of message content
         * @return Moved content
         */
        [[nodiscard]] RuntimeValue takeContent(void);

    private:
        ProcessId _senderPid;
        RuntimeValue _content;
    };

} 
