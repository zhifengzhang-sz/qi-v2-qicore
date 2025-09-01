/**
 * CLI State Manager Implementation
 *
 * XState 5-based state manager that handles hierarchical states:
 * - busy/ready primary states
 * - planning/editing/generic sub-states with Shift+Tab cycling
 */

import { createActor } from 'xstate'
import type {
  AppState,
  AppStateContext,
  AppSubState,
  IStateManager,
  StateCallback,
  StateEvent,
  StateMachineSnapshot,
} from '../abstractions/index'
import {
  type CLIStateActor,
  type CLIStateContext,
  type CLIStateEvent,
  canCycleStates,
  cliStateMachine,
  createInitialContext,
  getStateDescription,
} from '../abstractions/state-machine'

// ============================================================================
// State Manager Implementation
// ============================================================================

export class CLIStateManager implements IStateManager {
  private actor: CLIStateActor
  private subscribers: Set<StateCallback> = new Set()

  constructor(sessionId: string = this.generateSessionId()) {
    // Create actor with initial context
    const initialContext = createInitialContext(sessionId)

    this.actor = createActor(cliStateMachine, {
      input: initialContext,
    })

    // Subscribe to state changes and notify subscribers
    this.actor.subscribe((snapshot) => {
      const context = this.mapSnapshotToContext(snapshot)
      this.notifySubscribers(context)
    })

    // Start the actor
    this.actor.start()
  }

  // ==========================================================================
  // State Access Methods
  // ==========================================================================

  getCurrentState(): AppState {
    return this.actor.getSnapshot().context.currentState
  }

  getCurrentSubState(): AppSubState {
    return this.actor.getSnapshot().context.currentSubState
  }

  getStateContext(): AppStateContext {
    return this.mapSnapshotToContext(this.actor.getSnapshot())
  }

  // ==========================================================================
  // State Transition Methods
  // ==========================================================================

  transition(event: StateEvent): void {
    // Map generic StateEvent to CLIStateEvent
    const cliEvent = this.mapToCLIEvent(event)
    if (cliEvent) {
      this.actor.send(cliEvent)
    }
  }

  canCycleStates(): boolean {
    const context = this.actor.getSnapshot().context
    return canCycleStates(context)
  }

  cycleReadyStates(): void {
    if (!this.canCycleStates()) {
      console.debug('Cannot cycle states - system is busy')
      return
    }

    this.actor.send({ type: 'CYCLE_STATES' })
  }

  setBusy(taskName: string): void {
    this.actor.send({
      type: 'START_TASK',
      taskName,
    })
  }

  setReady(subState?: AppSubState): void {
    if (subState) {
      this.actor.send({
        type: 'SET_STATE',
        subState,
      })
    }

    this.actor.send({ type: 'TASK_COMPLETE' })
  }

  // ==========================================================================
  // Subscription Methods
  // ==========================================================================

  subscribe(callback: StateCallback): () => void {
    this.subscribers.add(callback)

    // Immediately call with current state
    const context = this.getStateContext()
    callback(context)

    // Return unsubscribe function
    return () => {
      this.subscribers.delete(callback)
    }
  }

  // ==========================================================================
  // Debug Methods
  // ==========================================================================

  getMachineSnapshot(): StateMachineSnapshot {
    const snapshot = this.actor.getSnapshot()
    const context = this.mapSnapshotToContext(snapshot)

    return {
      value: snapshot.value as string,
      context,
      canTransition: (event: StateEvent) => {
        const cliEvent = this.mapToCLIEvent(event)
        return cliEvent ? snapshot.can(cliEvent) : false
      },
    }
  }

  // ==========================================================================
  // Utility Methods
  // ==========================================================================

  /**
   * Get human-readable state description
   */
  getStateDescription(): string {
    const context = this.actor.getSnapshot().context
    return getStateDescription(context)
  }

  /**
   * Handle task error
   */
  handleTaskError(error: string): void {
    this.actor.send({
      type: 'TASK_ERROR',
      error,
    })
  }

  /**
   * Reset to initial state
   */
  reset(): void {
    this.actor.send({ type: 'RESET' })
  }

  /**
   * Stop the state manager
   */
  stop(): void {
    this.actor.stop()
    this.subscribers.clear()
  }

  // ==========================================================================
  // Private Helper Methods
  // ==========================================================================

  private generateSessionId(): string {
    return `cli-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }

  private mapSnapshotToContext(
    snapshot: Parameters<Parameters<CLIStateActor['subscribe']>[0]>[0]
  ): AppStateContext {
    const context = snapshot.context as CLIStateContext

    return {
      currentState: context.currentState,
      currentSubState: context.currentSubState,
      lastSubState: context.lastSubState,
      taskName: context.taskName,
      startTime: context.startTime,
    }
  }

  private mapToCLIEvent(event: StateEvent): CLIStateEvent | null {
    switch (event.type) {
      case 'START_TASK':
        return {
          type: 'START_TASK',
          taskName: (event.payload as { taskName: string })?.taskName || 'Unknown Task',
        }

      case 'TASK_COMPLETE':
        return { type: 'TASK_COMPLETE' }

      case 'TASK_ERROR':
        return {
          type: 'TASK_ERROR',
          error: (event.payload as { error: string })?.error,
        }

      case 'CYCLE_STATES':
        return { type: 'CYCLE_STATES' }

      case 'SET_STATE': {
        const subState = (event.payload as { subState: AppSubState })?.subState
        if (subState) {
          return { type: 'SET_STATE', subState }
        }
        return null
      }

      case 'RESET':
        return { type: 'RESET' }

      default:
        console.warn(`Unknown state event type: ${event.type}`)
        return null
    }
  }

  private notifySubscribers(context: AppStateContext, event?: StateEvent): void {
    for (const callback of this.subscribers) {
      try {
        callback(context, event)
      } catch (error) {
        console.error('Error in state change callback:', error)
      }
    }
  }
}

// ============================================================================
// Factory Functions
// ============================================================================

/**
 * Create a state manager with optional session ID
 */
export function createStateManager(sessionId?: string): CLIStateManager {
  return new CLIStateManager(sessionId)
}

/**
 * Create a state manager with debugging enabled
 */
export function createDebugStateManager(sessionId?: string): CLIStateManager {
  const manager = new CLIStateManager(sessionId)

  // Add debug logging
  manager.subscribe((context: AppStateContext, event?: StateEvent) => {
    console.debug(`State changed: ${manager.getStateDescription()}`, {
      context,
      event,
    })
  })

  return manager
}
