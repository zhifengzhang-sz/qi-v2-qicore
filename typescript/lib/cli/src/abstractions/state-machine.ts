/**
 * XState 5 CLI State Machine Definition
 *
 * Hierarchical state machine for CLI application with:
 * - Primary states: busy, ready
 * - Secondary states (in ready): planning, editing, generic
 * - Shift+Tab cycling only when ready
 */

import { type ActorRefFrom, assign, setup } from 'xstate'
import type { AppState, AppStateContext, AppSubState } from './cli-interfaces'

// ============================================================================
// State Machine Events
// ============================================================================

export type CLIStateEvent =
  | { type: 'START_TASK'; taskName: string }
  | { type: 'TASK_COMPLETE' }
  | { type: 'TASK_ERROR'; error?: string }
  | { type: 'CYCLE_STATES' }
  | { type: 'SET_STATE'; subState: AppSubState }
  | { type: 'RESET' }

// ============================================================================
// State Machine Context
// ============================================================================

export interface CLIStateContext extends AppStateContext {
  readonly taskName?: string
  readonly startTime?: Date
  readonly error?: string
  readonly sessionId: string
}

// ============================================================================
// State Machine Definition
// ============================================================================

const cliStateMachineSetup = setup({
  types: {} as {
    context: CLIStateContext
    events: CLIStateEvent
  },
  actions: {
    setBusyState: assign({
      currentState: 'busy' as AppState,
      startTime: () => new Date(),
    }),

    setReadyState: assign({
      currentState: 'ready' as AppState,
    }),

    setPlanningMode: assign({
      currentSubState: 'planning' as AppSubState,
    }),

    setEditingMode: assign({
      currentSubState: 'editing' as AppSubState,
    }),

    setGenericMode: assign({
      currentSubState: 'generic' as AppSubState,
    }),

    saveCurrentSubState: assign({
      lastSubState: ({ context }) => context.currentSubState,
    }),

    restoreLastSubState: assign({
      currentSubState: ({ context }) => context.lastSubState,
    }),

    setTaskName: assign({
      taskName: ({ event }) => (event.type === 'START_TASK' ? event.taskName : undefined),
      startTime: ({ event }) => (event.type === 'START_TASK' ? new Date() : undefined),
    }),

    clearTask: assign({
      taskName: undefined,
      error: undefined,
    }),

    handleTaskError: assign({
      error: ({ event }) =>
        event.type === 'TASK_ERROR' ? event.error || 'Unknown task error' : undefined,
    }),

    setExplicitSubState: assign(({ context, event }) => {
      if (event.type === 'SET_STATE') {
        return {
          currentSubState: event.subState,
          lastSubState: context.currentSubState,
        }
      }
      return {}
    }),

    resetContext: assign({
      currentState: 'ready' as AppState,
      currentSubState: 'generic' as AppSubState,
      lastSubState: 'generic' as AppSubState,
      taskName: undefined,
      error: undefined,
      startTime: () => new Date(),
    }),

    ignoreCycleRequest: () => {
      // Log or handle ignored cycle request when busy
      console.debug('State cycling ignored - system is busy')
    },
  },
})

export const cliStateMachine = cliStateMachineSetup.createMachine({
  id: 'cli',

  initial: 'ready',

  context: {
    currentState: 'ready' as AppState,
    currentSubState: 'generic' as AppSubState,
    lastSubState: 'generic' as AppSubState,
    sessionId: '',
    startTime: new Date(),
  },

  states: {
    // ========================================================================
    // Busy State - Processing tasks
    // ========================================================================
    busy: {
      entry: ['setBusyState'],
      exit: ['clearTask'],

      on: {
        TASK_COMPLETE: {
          target: 'ready',
          actions: ['restoreLastSubState'],
        },

        TASK_ERROR: {
          target: 'ready',
          actions: ['handleTaskError', 'restoreLastSubState'],
        },

        // Ignore state cycling when busy
        CYCLE_STATES: {
          actions: ['ignoreCycleRequest'],
        },
      },
    },

    // ========================================================================
    // Ready State - Available for interaction
    // ========================================================================
    ready: {
      entry: ['setReadyState'],
      initial: 'generic',

      states: {
        // Planning mode - strategic thinking, workflow planning
        planning: {
          entry: ['setPlanningMode'],
          on: {
            CYCLE_STATES: {
              target: 'editing',
              actions: ['saveCurrentSubState'],
            },
          },
        },

        // Editing mode - code editing, file modifications
        editing: {
          entry: ['setEditingMode'],
          on: {
            CYCLE_STATES: {
              target: 'generic',
              actions: ['saveCurrentSubState'],
            },
          },
        },

        // Generic mode - general conversation
        generic: {
          entry: ['setGenericMode'],
          on: {
            CYCLE_STATES: {
              target: 'planning',
              actions: ['saveCurrentSubState'],
            },
          },
        },
      },

      on: {
        START_TASK: {
          target: 'busy',
          actions: ['saveCurrentSubState', 'setTaskName'],
        },

        SET_STATE: {
          actions: ['setExplicitSubState'],
        },
      },
    },
  },

  on: {
    RESET: {
      target: '.ready.generic',
      actions: ['resetContext'],
    },
  },
})

// ============================================================================
// State Machine Actor Type
// ============================================================================

export type CLIStateActor = ActorRefFrom<typeof cliStateMachine>

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Get readable state description
 */
export function getStateDescription(context: CLIStateContext): string {
  if (context.currentState === 'busy') {
    return context.taskName ? `Busy: ${context.taskName}` : 'Busy'
  }

  const subStateLabels = {
    planning: 'Planning Mode',
    editing: 'Editing Mode',
    generic: 'Generic Mode',
  }

  return `Ready: ${subStateLabels[context.currentSubState]}`
}

/**
 * Check if state cycling is allowed
 */
export function canCycleStates(context: CLIStateContext): boolean {
  return context.currentState === 'ready'
}

/**
 * Get next sub-state in cycle
 */
export function getNextSubState(current: AppSubState): AppSubState {
  const cycle: AppSubState[] = ['generic', 'planning', 'editing']
  const currentIndex = cycle.indexOf(current)
  return cycle[(currentIndex + 1) % cycle.length]
}

/**
 * Create initial context with session ID
 */
export function createInitialContext(sessionId: string): CLIStateContext {
  return {
    currentState: 'ready',
    currentSubState: 'generic',
    lastSubState: 'generic',
    sessionId,
    startTime: new Date(),
  }
}
