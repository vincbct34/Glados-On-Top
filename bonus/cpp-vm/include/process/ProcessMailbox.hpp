/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** ProcessMailbox - Thread-safe message queue for processes
*/

#pragma once

#include <queue>
#include <mutex>
#include <optional>
#include "process/Message.hpp"

namespace rat
{
    /**
     * @class ProcessMailbox
     * @brief Thread-safe mailbox for process messages
     *
     * Infrastructure component that provides thread-safe message queuing
     * and retrieval for processes. Single Responsibility: Message queueing.
     *
     * Thread Safety: All operations are protected by a mutex.
     *
     * @author Robin Toillon
     */
    class ProcessMailbox {
    public:
        /**
         * @brief Construct an empty mailbox
         */
        ProcessMailbox(void)= default;

        // Disable copy/move for thread safety
        ProcessMailbox(const ProcessMailbox&) = delete;
        ProcessMailbox& operator=(const ProcessMailbox&) = delete;
        ProcessMailbox(ProcessMailbox&&) = delete;
        ProcessMailbox& operator=(ProcessMailbox&&) = delete;

        /**
         * @brief Enqueue a message
         * @param msg Message to add to the queue
         */
        void enqueue(Message msg);

        /**
         * @brief Dequeue the oldest message
         * @return Message if available, nullopt if empty
         */
        [[nodiscard]] std::optional<Message> dequeue(void);

        /**
         * @brief Check if mailbox is empty
         * @return true if no messages
         */
        [[nodiscard]] bool isEmpty(void) const;

        /**
         * @brief Get number of messages in mailbox
         * @return Message count
         */
        [[nodiscard]] size_t size(void) const;

        /**
         * @brief Clear all messages
         */
        void clear(void);

    private:
        mutable std::mutex _mutex;
        std::queue<Message> _messages;
    };

} 
