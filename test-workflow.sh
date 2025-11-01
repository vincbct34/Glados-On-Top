#!/bin/bash
set -e  # Exit on error

echo "🧪 Testing GitHub Pages workflow locally..."
echo ""

# Step 1: Coverage
echo "📊 Step 1: Generating coverage..."
make coverage
if [ ! -f "coverage/index.html" ]; then
    echo "❌ FAILED: coverage/index.html not found"
    exit 1
fi
echo "✅ Coverage generated successfully"
echo ""

# Step 2: Haddock
echo "📚 Step 2: Generating Haddock documentation..."
stack haddock --no-haddock-deps
HADDOCK_DIR=$(find .stack-work/install -type d -name doc -path "*/Glados-On-Top-*/doc" | head -1)
if [ -z "$HADDOCK_DIR" ]; then
    HADDOCK_DIR=$(find .stack-work/install -type d -name doc | head -1)
fi
if [ ! -f "$HADDOCK_DIR/index.html" ]; then
    echo "❌ FAILED: Haddock index.html not found"
    echo "Searched for: $HADDOCK_DIR/index.html"
    exit 1
fi
echo "✅ Haddock generated at: $HADDOCK_DIR"
echo ""

# Step 3: Prepare gh-pages
echo "📦 Step 3: Preparing GitHub Pages content..."
rm -rf gh-pages-test
mkdir -p gh-pages-test

cp -r coverage gh-pages-test/coverage
cp -r "$HADDOCK_DIR" gh-pages-test/docs

# Create index
cat > gh-pages-test/index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>GLaDOS/Ratatouille Documentation</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            line-height: 1.6;
            color: #333;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 20px;
        }
        .container {
            max-width: 800px;
            background: white;
            border-radius: 20px;
            padding: 40px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
        }
        h1 {
            color: #667eea;
            margin-bottom: 10px;
            font-size: 2.5em;
        }
        .subtitle {
            color: #666;
            margin-bottom: 30px;
            font-size: 1.1em;
        }
        .links {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-top: 30px;
        }
        .link-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 15px;
            text-decoration: none;
            transition: transform 0.3s, box-shadow 0.3s;
            display: flex;
            flex-direction: column;
        }
        .link-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 10px 30px rgba(102, 126, 234, 0.4);
        }
        .link-card h2 {
            font-size: 1.5em;
            margin-bottom: 10px;
        }
        .link-card p {
            opacity: 0.9;
            font-size: 0.95em;
        }
        .version {
            margin-top: 30px;
            text-align: center;
            color: #999;
            font-size: 0.9em;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>GLaDOS/Ratatouille</h1>
        <p class="subtitle">Actor-Model Programming Language</p>

        <div class="links">
            <a href="docs/index.html" class="link-card">
                <h2>📚 API Documentation</h2>
                <p>Haddock-generated API reference for all modules</p>
            </a>

            <a href="coverage/index.html" class="link-card">
                <h2>📊 Test Coverage</h2>
                <p>HPC coverage reports for the test suite</p>
            </a>
        </div>

        <p class="version">Version 3.0.0 | Built with Stack + GHC 9.10.2</p>
    </div>
</body>
</html>
EOF

echo "✅ GitHub Pages content prepared in gh-pages-test/"
echo ""

# Step 4: Verify
echo "🔍 Step 4: Verifying structure..."
echo "Directory structure:"
ls -la gh-pages-test/
echo ""
echo "Coverage files:"
ls -la gh-pages-test/coverage/ | head -5
echo ""
echo "Documentation files:"
ls -la gh-pages-test/docs/ | head -5
echo ""

if [ -f "gh-pages-test/index.html" ] && \
   [ -f "gh-pages-test/docs/index.html" ] && \
   [ -f "gh-pages-test/coverage/index.html" ]; then
    echo "✅ ALL CHECKS PASSED!"
    echo ""
    echo "You can view the test site by opening:"
    echo "  file://$(pwd)/gh-pages-test/index.html"
    echo ""
    echo "Or on Windows:"
    echo "  start gh-pages-test/index.html"
    echo ""
    echo "👍 Safe to push to main!"
    exit 0
else
    echo "❌ SOME FILES ARE MISSING!"
    echo ""
    echo "Checking what's missing:"
    [ ! -f "gh-pages-test/index.html" ] && echo "  ✗ gh-pages-test/index.html"
    [ ! -f "gh-pages-test/docs/index.html" ] && echo "  ✗ gh-pages-test/docs/index.html"
    [ ! -f "gh-pages-test/coverage/index.html" ] && echo "  ✗ gh-pages-test/coverage/index.html"
    exit 1
fi
