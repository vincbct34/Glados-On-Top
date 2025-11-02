/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** ProcessMailbox Implementation
*/

#include "process/ProcessMailbox.hpp"

namespace rat
{
    void ProcessMailbox::enqueue(Message msg) {
        std::lock_guard lock(_mutex);
        _messages.push(std::move(msg));
    }

    std::optional<Message> ProcessMailbox::dequeue(void){
        std::lock_guard lock(_mutex);
        if (_messages.empty()) {
            return std::nullopt;
        }
        Message msg = std::move(_messages.front());
        _messages.pop();
        return msg;
    }

    bool ProcessMailbox::isEmpty(void) const{
        std::lock_guard lock(_mutex);
        return _messages.empty();
    }

    size_t ProcessMailbox::size(void) const{
        std::lock_guard lock(_mutex);
        return _messages.size();
    }

    void ProcessMailbox::clear(void){
        std::lock_guard lock(_mutex);
        std::queue<Message> empty;
        std::swap(_messages, empty);
    }

} 
