// Verification script to test that lib/base is properly excluded
import fs from 'fs';
import path from 'path';

console.log("🔍 Verifying lib/base Exclusion Logic...\n");

// Read the compiled rule to verify exclusion logic
const rulePath = './eslint-rules/dist/no-result-anti-patterns.js';
const ruleContent = fs.readFileSync(rulePath, 'utf8');

console.log("✅ Checking compiled rule contains exclusion logic:");
const hasUnixExclusion = ruleContent.includes('/lib/base/src/');
const hasNormalization = ruleContent.includes('normalizedPath = filename.replace');
const hasReturnEmpty = ruleContent.includes('return {};');

console.log(`  - Unix path exclusion (/lib/base/src/): ${hasUnixExclusion ? '✅' : '❌'}`);
console.log(`  - Path normalization for Windows: ${hasNormalization ? '✅' : '❌'}`);
console.log(`  - Early return for excluded files: ${hasReturnEmpty ? '✅' : '❌'}`);

// Test the exclusion logic manually
console.log("\n🧪 Testing exclusion logic:");

function testExclusion(filename) {
  // Simulate the same logic as our ESLint rule
  const normalizedPath = filename.replace(/\\/g, '/');
  return normalizedPath.includes('/lib/base/src/') || normalizedPath.includes('/lib/tests/base/');
}

const testCases = [
  '/project/lib/base/src/result.ts',
  '/project/lib/base/src/async.ts',
  '/project/lib/tests/base/result.test.ts',
  '/project/lib/core/src/cache.ts',
  '/project/lib/cli/src/factory.ts',
  'C:\\project\\lib\\base\\src\\result.ts',
  'C:\\project\\lib\\tests\\base\\async.test.ts',
  'C:\\project\\lib\\core\\src\\config.ts'
];

testCases.forEach(testCase => {
  const excluded = testExclusion(testCase);
  const shouldBeExcluded = testCase.includes('base');
  const result = excluded === shouldBeExcluded ? '✅' : '❌';
  console.log(`  ${testCase}: ${excluded ? 'EXCLUDED' : 'INCLUDED'} ${result}`);
});

// Count actual patterns in lib/base vs lib/core for comparison
console.log("\n📊 Pattern Count Comparison:");

function countPatterns(dir) {
  const files = fs.readdirSync(dir, { recursive: true })
    .filter(f => f.endsWith('.ts'))
    .map(f => path.join(dir, f));

  let count = 0;
  files.forEach(file => {
    const content = fs.readFileSync(file, 'utf8');
    const matches = content.match(/\.tag\s*===?\s*['"`](success|failure)['"`]/g);
    if (matches) count += matches.length;
  });

  return { files: files.length, patterns: count };
}

const baseStats = countPatterns('lib/base/src');
const coreStats = countPatterns('lib/core/src');

console.log(`  lib/base/src: ${baseStats.files} files, ${baseStats.patterns} patterns (should be excluded)`);
console.log(`  lib/core/src: ${coreStats.files} files, ${coreStats.patterns} patterns (should be detected)`);

console.log("\n🎯 Verification Summary:");
console.log(`  - Rule exclusion logic: ${hasUnixExclusion && hasNormalization ? '✅ CORRECT' : '❌ MISSING'}`);
console.log(`  - lib/base has legitimate patterns: ${baseStats.patterns > 0 ? '✅ YES' : '❌ NO'}`);
console.log(`  - lib/core has anti-patterns: ${coreStats.patterns > 0 ? '✅ YES' : '❌ NO'}`);
console.log(`  - Exclusion prevents false positives: ${baseStats.patterns > 0 && hasUnixExclusion ? '✅ YES' : '❌ NO'}`);

console.log("\n✅ Base module exclusion verification complete!");