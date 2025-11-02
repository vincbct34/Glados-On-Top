/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Handler Registration - Register all instruction handlers
*/

#pragma once

#include "InstructionDispatcher.hpp"
#include "handlers/StackHandlers.hpp"
#include "handlers/ArithmeticHandlers.hpp"
#include "handlers/ComparisonHandlers.hpp"
#include "handlers/VariableHandlers.hpp"
#include "handlers/ControlFlowHandlers.hpp"

namespace rat
{
    /**
     * @brief Register all instruction handlers with the dispatcher
     * Follows Open/Closed Principle - easy to add new handlers
     */
    inline void registerAllHandlers(InstructionDispatcher& dispatcher)
    {
        // Stack operations
        dispatcher.registerHandler(handlers::handlePushInt);
        dispatcher.registerHandler(handlers::handlePushFloat);
        dispatcher.registerHandler(handlers::handlePushString);
        dispatcher.registerHandler(handlers::handlePushAtom);
        dispatcher.registerHandler(handlers::handlePushTuple);
        dispatcher.registerHandler(handlers::handlePushArray);
        dispatcher.registerHandler(handlers::handlePushUnit);
        dispatcher.registerHandler(handlers::handlePushBool);
        dispatcher.registerHandler(handlers::handlePushNone);
        dispatcher.registerHandler(handlers::handlePopN);
        dispatcher.registerHandler(handlers::handleDup);

        // Arithmetic operations
        dispatcher.registerHandler(handlers::handleAdd);
        dispatcher.registerHandler(handlers::handleSub);
        dispatcher.registerHandler(handlers::handleMul);
        dispatcher.registerHandler(handlers::handleDiv);
        dispatcher.registerHandler(handlers::handleNegate);
        dispatcher.registerHandler(handlers::handleConcat);
        dispatcher.registerHandler(handlers::handleIncVar);
        dispatcher.registerHandler(handlers::handleDecVar);
        dispatcher.registerHandler(handlers::handleIncVarPost);
        dispatcher.registerHandler(handlers::handleDecVarPost);

        // Comparison and logic operations
        dispatcher.registerHandler(handlers::handleCmpEq);
        dispatcher.registerHandler(handlers::handleCmpNeq);
        dispatcher.registerHandler(handlers::handleCmpLt);
        dispatcher.registerHandler(handlers::handleCmpLe);
        dispatcher.registerHandler(handlers::handleCmpGt);
        dispatcher.registerHandler(handlers::handleCmpGe);
        dispatcher.registerHandler(handlers::handleLogicAnd);
        dispatcher.registerHandler(handlers::handleLogicOr);
        dispatcher.registerHandler(handlers::handleLogicNot);

        // Variable and storage operations
        dispatcher.registerHandler(handlers::handleLoadVar);
        dispatcher.registerHandler(handlers::handleStoreVar);
        dispatcher.registerHandler(handlers::handleLoadLocal);
        dispatcher.registerHandler(handlers::handleStoreLocal);
        dispatcher.registerHandler(handlers::handleInitState);
        dispatcher.registerHandler(handlers::handleGetState);
        dispatcher.registerHandler(handlers::handleSetState);
        dispatcher.registerHandler(handlers::handleIndex);
        dispatcher.registerHandler(handlers::handleArrayLength);
        dispatcher.registerHandler(handlers::handleGetField);

        // Control flow
        dispatcher.registerHandler(handlers::handleJump);
        dispatcher.registerHandler(handlers::handleJumpIfFalse);
        dispatcher.registerHandler(handlers::handleJumpIfTrue);
        dispatcher.registerHandler(handlers::handleLabel);
        dispatcher.registerHandler(handlers::handleCall);
        dispatcher.registerHandler(handlers::handleReturn);
        dispatcher.registerHandler(handlers::handleHalt);
        dispatcher.registerHandler(handlers::handlePrint);

        // Function management
        dispatcher.registerHandler(handlers::handleDefineFunction);
        dispatcher.registerHandler(handlers::handleCallFunction);

        // Pattern matching
        dispatcher.registerHandler(handlers::handleMatchAtom);
        dispatcher.registerHandler(handlers::handleMatchVar);
        dispatcher.registerHandler(handlers::handleMatchTuple);
        dispatcher.registerHandler(handlers::handleMatchWildcard);
        dispatcher.registerHandler(handlers::handleMatchInt);
        dispatcher.registerHandler(handlers::handleMatchBool);
        dispatcher.registerHandler(handlers::handleMatchString);

        // Monadic operations
        dispatcher.registerHandler(handlers::handlePushJust);
        dispatcher.registerHandler(handlers::handlePushLeft);
        dispatcher.registerHandler(handlers::handlePushRight);
        dispatcher.registerHandler(handlers::handleMaybeBind);
        dispatcher.registerHandler(handlers::handleEitherBind);

        // Process management
        dispatcher.registerHandler(handlers::handleDefineProcess);
        dispatcher.registerHandler(handlers::handleCreateInstance);
        dispatcher.registerHandler(handlers::handleSend);
        dispatcher.registerHandler(handlers::handleWaitMessage);
        dispatcher.registerHandler(handlers::handleProcessLoop);
        dispatcher.registerHandler(handlers::handleSelf);
        dispatcher.registerHandler(handlers::handleExitProcess);

        // Type casting
        dispatcher.registerHandler(handlers::handleStaticCast);
        dispatcher.registerHandler(handlers::handleReinterpretCast);
        dispatcher.registerHandler(handlers::handleConstCast);
    }

}
