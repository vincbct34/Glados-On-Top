/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** IProcessManager - Interface for process/actor management
*/

#pragma once

#include <expected>
#include <string>
#include <vector>
#include "common/ErrorCode.hpp"
#include "core/InstructionSequence.hpp"
#include "process/ProcessIdentity.hpp"

namespace rat
{
    // Forward declaration
    class RuntimeValue;
}

namespace rat::interfaces
{
    /**
     * @interface IProcessManager
     * @brief Pure virtual interface for actor model process management
     *
     * Responsibility:
     * - Define and manage process templates (process definitions)
     * - Spawn new processes from templates with arguments
     * - Route messages between processes
     * - Manage process lifecycle and termination
     * - Track current execution context
     *
     * Design Pattern:
     * - Abstract Interface: Enables dependency injection and testing
     * - Actor Model: Processes communicate via asynchronous messaging
     *
     * Error Handling:
     * - All operations that can fail return std::expected<T, VMError>
     * - Errors provide detailed context through VMError types
     * - Never throws exceptions (noexcept guarantees where applicable)
     *
     * Thread Safety:
     * - Implementations MUST be thread-safe for concurrent process spawning
     * - Message passing between processes is atomic
     * - Process state access must be synchronized
     *
     * Implementation Notes:
     * - Uses ProcessIdentity (value object) instead of raw ProcessId types
     * - ProcessIdentity provides type-safe process identification
     * - Eliminates type alias confusion and improves code clarity
     *
     * @example
     * @code
     * auto manager = std::make_unique<ProcessManagerImpl>();
     *
     * // Define a process template
     * auto defineResult = manager->defineProcess("worker", {"param1"}, bytecode);
     * if (!defineResult) {
     *     std::cerr << "Failed to define process\n";
     *     return;
     * }
     *
     * // Spawn a process
     * std::vector<RuntimeValue> args{RuntimeValue(42)};
     * auto spawnResult = manager->spawn("worker", args);
     * if (!spawnResult) {
     *     std::cerr << "Failed to spawn process\n";
     *     return;
     * }
     * auto processId = std::move(*spawnResult);
     *
     * // Send a message
     * RuntimeValue message = RuntimeValue("Hello");
     * auto sendResult = manager->send(processId, message);
     * @endcode
     *
     * @see ProcessIdentity - Value object representing process identity
     * @see ProcessExecutionState - Mutable process runtime state
     * @see InterProcessMessage - Immutable message value object
     */
    class IProcessManager
    {
    public:
        virtual ~IProcessManager(void)= default;

        // ============================================================
        // Process Template Management
        // ============================================================

        /**
         * @brief Define a new process template for later spawning
         *
         * Creates a reusable process template that can be spawned multiple times.
         * Templates are immutable once defined and cached for efficiency.
         *
         * @param name Process template name (unique identifier)
         * @param params Parameter names that this process accepts
         * @param body Process bytecode (instructions to execute)
         * @return Success if template defined, error with details if failed
         *
         * @error InvalidOperand - Name is empty or contains invalid characters
         * @error InvalidState - Template with same name already exists
         * @error OutOfBounds - Too many parameters defined
         *
         * @note Process templates are immutable after definition
         * @note Template names must be globally unique
         * @note Parameter order matches spawn(void)argument order
         */
        [[nodiscard]]
        virtual std::expected<void, VMError> defineProcess(
            const std::string &name,
            const std::vector<std::string> &params,
            const core::InstructionSequence &body) = 0;

        // ============================================================
        // Process Lifecycle Management
        // ============================================================

        /**
         * @brief Spawn a new process from a template
         *
         * Creates a new process instance from a previously defined template,
         * binding the provided arguments to the template's parameters.
         *
         * @param name Process template name to spawn from
         * @param args Argument values to bind to template parameters
         * @return New process identity if successful, error if failed
         *
         * @error FunctionNotFound - Template with given name doesn't exist
         * @error InvalidArity - Argument count doesn't match parameter count
         * @error TypeMismatch - Argument types don't match expected types
         * @error InvalidState - Process pool is full or system exhausted
         *
         * @note Process IDs are unique and immutable
         * @note The main process always has ID 0
         * @note Spawned processes start in initialized state, not running
         *
         * @see ProcessIdentity for information on process IDs
         */
        [[nodiscard]]
        virtual std::expected<rat::process::ProcessIdentity, VMError> spawn(
            const std::string &name,
            const std::vector<RuntimeValue> &args) = 0;

        /**
         * @brief Terminate a running process
         *
         * Stops execution of a process and cleans up all associated resources.
         * The process is removed from the execution pool and can no longer receive messages.
         *
         * @param process Process identity to terminate
         * @return Success if terminated, error if process doesn't exist
         *
         * @error ProcessNotFound - Process with given ID doesn't exist
         * @error InvalidState - Process already terminated
         *
         * @note Terminating current process causes context switch to main
         * @note Pending messages in mailbox are discarded
         */
        [[nodiscard]]
        virtual std::expected<void, VMError> terminate(
            rat::process::ProcessIdentity process) = 0;

        // ============================================================
        // Inter-Process Communication (Message Passing)
        // ============================================================

        /**
         * @brief Send a message to a process
         *
         * Asynchronously sends a message to a process's mailbox.
         * The message is queued and delivered when the target process
         * executes a receive operation.
         *
         * @param recipient Process identity to send message to
         * @param message Message payload (can be any RuntimeValue)
         * @return Success if message queued, error if failed
         *
         * @error ProcessNotFound - Recipient process doesn't exist
         * @error MailboxFull - Recipient's mailbox is full (overflow)
         * @error InvalidState - Recipient process is terminated
         *
         * @note Message sending is non-blocking
         * @note Messages are delivered in FIFO order
         * @note Large messages are handled via unique_ptr (no copy)
         *
         * @see receive(void)- Retrieve messages
         */
        [[nodiscard]]
        virtual std::expected<void, VMError> send(
            rat::process::ProcessIdentity recipient,
            RuntimeValue message) = 0;

        /**
         * @brief Receive next message from process mailbox
         *
         * Retrieves the oldest message from the calling process's mailbox.
         * If mailbox is empty, returns error without blocking.
         *
         * @param process Process identity to receive for
         * @return Next message if available, error if mailbox empty or process missing
         *
         * @error ProcessNotFound - Process doesn't exist
         * @error MailboxEmpty - No messages available in mailbox
         * @error InvalidState - Process is terminated
         *
         * @note This operation is non-blocking
         * @note For blocking receive, caller must implement polling/waiting
         * @note Messages are removed from mailbox when retrieved
         *
         * @see send(void)- Send messages
         */
        [[nodiscard]]
        virtual std::expected<RuntimeValue, VMError> receive(
            rat::process::ProcessIdentity process) = 0;

        // ============================================================
        // Execution Context Management
        // ============================================================

        /**
         * @brief Get current executing process identity
         *
         * Returns the ProcessIdentity of the process currently being executed.
         * Used to track execution context during process switches.
         *
         * @return Current process identity
         *
         * @note Always returns a valid ProcessIdentity
         * @note Main process (ID 0) is returned when no other process is running
         * @note Called frequently during instruction execution
         */
        [[nodiscard]]
        virtual rat::process::ProcessIdentity getCurrentPid(void) const noexcept = 0;

        /**
         * @brief Set current executing process identity
         *
         * Switches the execution context to a different process.
         * Used when resuming process execution after context switch.
         *
         * @param process New current process to execute
         *
         * @warning Only call from VM core/scheduler
         * @note Must not be called from user-level code
         * @note Process must exist and be initialized
         * @note This is typically called by the scheduler, not directly
         */
        virtual void setCurrentPid(rat::process::ProcessIdentity process) noexcept = 0;
    };

}
