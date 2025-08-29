/**
 * Domain types for async composition example
 */

export interface AppConfig {
  app: {
    name: string
    version: string
    environment: string
  }
  api: {
    baseUrl: string
    timeout: number
    retries: number
  }
  database: {
    url: string
    poolSize: number
  }
  processing: {
    batchSize: number
    maxConcurrency: number
  }
  logging: {
    level: string
    pretty: boolean
  }
}

export interface User {
  id: number
  name: string
  email: string
  phone?: string
  website?: string
}

export interface Post {
  id: number
  userId: number
  title: string
  body: string
}

export interface UserProfile {
  user: User
  posts: Post[]
  postCount: number
  isActive: boolean
}

export interface ProcessingStats {
  processed: number
  succeeded: number
  failed: number
  startTime: Date
  endTime?: Date
  duration?: number
}
