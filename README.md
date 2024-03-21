# Markdown Parser in Haskell

Markdown parser implemented in Haskell using the Megaparsec library.

## Instructions

To use the Markdown Parser, follow these simple steps:

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/your-username/markdown-parser.git
   ```

2. **Build the Project:**
   ```bash
   cd markdown-parser
   cabal build
   ```

3. **Run the Parser:**
   ```bash
   cabal run
   ```
4. **Send POST request with JSON Data**
   ```
 curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"message": "# Hello\n## There."}' \
  http://localhost:3000/toHTML
   ```

   Replace `inputFile.md` with the path to your markdown file and `outputFile.html` with the desired name for the HTML output.

## Things to Work On

- **Indentation-sensitive Parsing**

## Contributing

If you'd like to contribute to the Markdown Parser project, please feel free to fork the repository, make your changes, and submit a pull request. Your contributions are valuable and appreciated!