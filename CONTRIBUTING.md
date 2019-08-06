# Contributing

Thanks for taking the time to contribute!

The following is a set of instructions and guidelines for contributing to the Open Quant Live Book Initiative. 

## Contribution Licensing

This work is licensed under [Attribution-NonCommercial-ShareAlike 4.0 International](https://creativecommons.org/licenses/by-nc-sa/4.0/). Hence, contributions that you make are licensed under the same terms.

## Which languanges should you use?

You can code in any language. However, you need to submit your Chapter in markdown or Rmarkdown.

## Minimum Criteria

Your chapter and results should be reproducible and you should comply with the License of the project.

## Project Structure

```
.
├── open-quant-live-book                           # Home directory
|   ├── _book                                      # Output book files (html/pdf/epub) directory
|   ├── chapters                                   # Book Chapters (markdown/rmarkdown)
|   ├── fig                                        # Figures that are common across the book
```

## Submitting a Chapter

### Step 1: Fork the repo

### Step 2: Create self-contained Chapter directory

Create a self-contained folder under /Chapters with the code and markdown/Rmarkdown file of your chapter.

### Step 3: Submit pull request

One of our admins will review your pull request and build the book. The output files (html for the ebook) will be available under the directory _book. It will also be automatically available at www.ebook.openquants.com.

## If you want to build the book yourself before submitting a pull request

### Sotfware Requirements

You should install the following software:

+ Bookdown
+ R
+ R-Studio

### Getting Started

1. Fork the project or download it as a .zip file
2. Open the bookdown project on R-Studio (bookdown-demo.Rproj)
3. Open the R Markdown file index.Rmd and click the button Build Book on the Build tab of RStudio (top right-hand corner).

You can build the book as a .pdf, epub, or gitbook (renders as a web-page in the gitbook style). The output files will be available in the folder _book.

### Integrating your new Chapter into the book

After you create your self-contained folder under /Chapters with the code and markdown/Rmarkdown file of your chapter, you need to add the path to the markdown file of your Chapter to the file _bookdown.yml located in the root folder of the repo. Now when you build the book it will contain your Chapter in it.




