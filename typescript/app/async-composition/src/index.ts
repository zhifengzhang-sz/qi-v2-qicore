/**
 * Async Result Composition Patterns Demo
 *
 * This example demonstrates the power of QiCore Foundation's async helpers
 * in real-world scenarios, showing before/after patterns for common tasks.
 */

import {
  success,
  match,
  flatMap,
  // Async composition helpers
  flatMapAsync,
  mapAsync,
  matchAsync,
  flatMapPromise,
  sequenceAsync,
  collectAsync,
  type QiError,
} from '@qi/base'

import {
  loadConfig,
  fetchUser,
  fetchUserPosts,
  saveUser,
  processUserData,
  sendNotification,
  validateConfig,
} from './services.js'

import type { UserProfile, ProcessingStats, AppConfig } from './types.js'

// ============================================================================
// Example 1: Before/After - Manual Unwrapping vs Async Composition
// ============================================================================

/**
 * ❌ ANTI-PATTERN: Manual Result unwrapping (the old way)
 */
async function loadUserProfileManual(userId: number): Promise<void> {
  console.log('\n🔴 ANTI-PATTERN: Manual unwrapping approach')

  // Load and validate configuration
  const configResult = await loadConfig()
  if (configResult.tag === 'failure') {
    console.error('❌ Config failed:', configResult.error.message)
    return
  }

  const validatedConfig = validateConfig(configResult.value)
  if (validatedConfig.tag === 'failure') {
    console.error('❌ Config validation failed:', validatedConfig.error.message)
    return
  }

  // Fetch user
  const userResult = await fetchUser(userId)
  if (userResult.tag === 'failure') {
    console.error('❌ User fetch failed:', userResult.error.message)
    return
  }

  // Process user data
  const processedResult = await processUserData(userResult.value)
  if (processedResult.tag === 'failure') {
    console.error('❌ User processing failed:', processedResult.error.message)
    return
  }

  // Fetch posts
  const postsResult = await fetchUserPosts(userId)
  if (postsResult.tag === 'failure') {
    console.error('❌ Posts fetch failed:', postsResult.error.message)
    return
  }

  // Save processed user
  const savedResult = await saveUser(processedResult.value)
  if (savedResult.tag === 'failure') {
    console.error('❌ Save failed:', savedResult.error.message)
    return
  }

  // Send notification
  const notificationResult = await sendNotification(
    savedResult.value,
    'Profile updated successfully'
  )
  if (notificationResult.tag === 'failure') {
    console.error('❌ Notification failed:', notificationResult.error.message)
    return
  }

  const profile: UserProfile = {
    user: savedResult.value,
    posts: postsResult.value,
    postCount: postsResult.value.length,
    isActive: true,
  }

  console.log('✅ Profile loaded successfully:', {
    user: profile.user.name,
    posts: profile.postCount,
    email: profile.user.email,
  })
}

/**
 * ✅ CLEAN: Functional async composition (the new way)
 */
async function loadUserProfileFunctional(userId: number): Promise<void> {
  console.log('\n🟢 CLEAN: Functional async composition')

  // Load and validate config with clean composition
  const configResult = await flatMapPromise((config) => validateConfig(config), loadConfig())

  // Chain the entire user processing pipeline
  await matchAsync(
    async (config) => {
      console.log(`🔧 Using config environment: ${config.app.environment}`)

      // Process user data pipeline
      const userProfileResult = await flatMapAsync(
        async (user) => {
          // Process user and fetch posts in parallel
          const [processedResult, postsResult] = await Promise.all([
            processUserData(user),
            fetchUserPosts(userId),
          ])

          // Combine results
          return flatMap(
            (processedUser) =>
              flatMap((posts) => {
                const profile: UserProfile = {
                  user: processedUser,
                  posts,
                  postCount: posts.length,
                  isActive: true,
                }
                return success(profile)
              }, postsResult),
            processedResult
          )
        },
        await fetchUser(userId)
      )

      // Handle the final result
      await matchAsync(
        async (profile) => {
          // Save user and send notification
          await flatMapAsync(
            (savedUser) => sendNotification(savedUser, 'Profile updated successfully'),
            await saveUser(profile.user)
          )

          console.log('✅ Profile loaded successfully:', {
            user: profile.user.name,
            posts: profile.postCount,
            email: profile.user.email,
          })
        },
        async (error) => {
          console.error('❌ Profile processing failed:', error.message)
        },
        userProfileResult
      )
    },
    async (error) => {
      console.error('❌ Configuration failed:', error.message)
    },
    configResult
  )
}

// ============================================================================
// Example 2: Collection Operations - Processing Multiple Users
// ============================================================================

/**
 * Process multiple users with different strategies
 */
async function processMultipleUsers(): Promise<void> {
  console.log('\n📊 Collection Operations Examples')

  const userIds = [1, 2, 3, 4, 999] // 999 will fail

  // Strategy 1: Sequential processing (stop at first failure)
  console.log('\n🔄 Sequential processing (sequenceAsync)')
  const sequentialResult = await sequenceAsync(userIds.map((id) => fetchUser(id)))

  match(
    (users) => console.log(`✅ Sequential: Loaded ${users.length} users`),
    (error) => console.log(`❌ Sequential failed at: ${error.message}`),
    sequentialResult
  )

  // Strategy 2: Collect all results (partition successes/failures)
  console.log('\n📦 Parallel processing (collectAsync)')
  const { successes: users, failures: errors } = await collectAsync(
    userIds.map((id) => fetchUser(id))
  )

  console.log(`✅ Parallel: ${users.length} users loaded, ${errors.length} failed`)
  for (const error of errors) {
    console.log(`  ❌ ${error.message}`)
  }

  // Strategy 3: Process successful users further
  if (users.length > 0) {
    console.log('\n⚡ Processing successful users')
    const processedResults = await collectAsync(users.map((user) => processUserData(user)))

    console.log(
      `✅ Processed: ${processedResults.successes.length} users, ${processedResults.failures.length} failed`
    )
  }
}

// ============================================================================
// Example 3: Advanced Composition - User Data Pipeline
// ============================================================================

/**
 * Complex async pipeline with error recovery
 */
async function advancedUserPipeline(): Promise<void> {
  console.log('\n🔧 Advanced Pipeline Example')

  const stats: ProcessingStats = {
    processed: 0,
    succeeded: 0,
    failed: 0,
    startTime: new Date(),
  }

  // Load configuration with validation
  const pipeline = await flatMapPromise(
    (config) => {
      console.log(`🔧 Pipeline configured for ${config.app.environment}`)
      return success(config)
    },
    flatMapPromise(validateConfig, loadConfig())
  )

  await matchAsync(
    async (config: AppConfig) => {
      // Process users in batches
      const userIds = [1, 2, 3, 4, 5, 6, 7]
      const batchSize = config.processing.batchSize

      for (let i = 0; i < userIds.length; i += batchSize) {
        const batch = userIds.slice(i, i + batchSize)
        console.log(`\n🔄 Processing batch: ${batch.join(', ')}`)

        // Process batch with error recovery
        const batchResults = await collectAsync(
          batch.map(async (id) => {
            stats.processed++

            return await flatMapAsync(
              async (user) => {
                // Multi-step processing pipeline
                const processedResult = await flatMapAsync(
                  async (processedUser) => {
                    const savedResult = await saveUser(processedUser)
                    return await mapAsync(async (savedUser) => {
                      await sendNotification(savedUser, 'Welcome to the system!')
                      return savedUser
                    }, savedResult)
                  },
                  await processUserData(user)
                )

                return processedResult
              },
              await fetchUser(id)
            )
          })
        )

        stats.succeeded += batchResults.successes.length
        stats.failed += batchResults.failures.length

        console.log(
          `  ✅ Batch complete: ${batchResults.successes.length} succeeded, ${batchResults.failures.length} failed`
        )

        // Log any failures
        for (const error of batchResults.failures) {
          console.log(`    ❌ ${error.message}`)
        }
      }

      stats.endTime = new Date()
      stats.duration = stats.endTime.getTime() - stats.startTime.getTime()

      console.log('\n📊 Pipeline Statistics:')
      console.log(`  📈 Total processed: ${stats.processed}`)
      console.log(`  ✅ Succeeded: ${stats.succeeded}`)
      console.log(`  ❌ Failed: ${stats.failed}`)
      console.log(`  ⏱️  Duration: ${stats.duration}ms`)
      console.log(`  📋 Success rate: ${((stats.succeeded / stats.processed) * 100).toFixed(1)}%`)
    },
    async (error: QiError) => {
      console.error('❌ Pipeline configuration failed:', error.message)
    },
    pipeline
  )
}

// ============================================================================
// Example 4: Error Handling Strategies
// ============================================================================

/**
 * Demonstrate different error handling approaches
 */
async function errorHandlingStrategies(): Promise<void> {
  console.log('\n🛡️ Error Handling Strategies')

  // Strategy 1: Fail fast with matchAsync
  console.log('\n⚡ Fail fast strategy')
  await matchAsync(
    async (user) => {
      console.log(`✅ User loaded: ${user.name}`)

      // Chain operations that might fail
      await matchAsync(
        async (posts) => console.log(`✅ Posts loaded: ${posts.length}`),
        async (error) => console.log(`❌ Posts failed: ${error.message}`),
        await fetchUserPosts(user.id)
      )
    },
    async (error) => {
      console.log(`❌ User load failed: ${error.message}`)
    },
    await fetchUser(1)
  )

  // Strategy 2: Graceful degradation
  console.log('\n🔄 Graceful degradation strategy')
  const userResult = await fetchUser(2)
  const postsResult = await fetchUserPosts(2)

  // Always create a profile, even with partial data
  const profile = match(
    (user) =>
      match(
        (posts) => ({ user, posts, postCount: posts.length, isActive: true }),
        (_error) => ({ user, posts: [], postCount: 0, isActive: true }),
        postsResult
      ),
    (error) => {
      console.log(`⚠️ Using fallback user due to error: ${error.message}`)
      return {
        user: { id: -1, name: 'Unknown User', email: 'unknown@example.com' },
        posts: [],
        postCount: 0,
        isActive: false,
      }
    },
    userResult
  )

  console.log('✅ Profile created with graceful degradation:', {
    name: profile.user.name,
    posts: profile.postCount,
    active: profile.isActive,
  })
}

// ============================================================================
// Main Demo Function
// ============================================================================

async function runDemo(): Promise<void> {
  console.log('🚀 QiCore Foundation - Async Composition Patterns Demo')
  console.log('=====================================================')

  try {
    // Example 1: Before/After comparison
    await loadUserProfileManual(1)
    await loadUserProfileFunctional(1)

    // Example 2: Collection operations
    await processMultipleUsers()

    // Example 3: Advanced pipeline
    await advancedUserPipeline()

    // Example 4: Error handling strategies
    await errorHandlingStrategies()

    console.log('\n🎉 Demo completed successfully!')
    console.log('\n💡 Key Takeaways:')
    console.log('  • Async helpers eliminate manual Result unwrapping')
    console.log('  • Functional composition is cleaner and more maintainable')
    console.log('  • Error handling becomes declarative and composable')
    console.log('  • Collection operations provide flexible processing strategies')
    console.log('  • Complex pipelines remain readable and type-safe')
  } catch (error) {
    console.error('💥 Demo failed with unexpected error:', error)
  }
}

// Run the demo
if (import.meta.url === new URL(process.argv[1] || '', 'file:').href) {
  runDemo()
}
