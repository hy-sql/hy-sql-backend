const splitCommandIntoArray = (commandString) => {
    return commandString
        .trim()
        .replace(/\s\s+/g, ' ')
        .replace(/\s+,/g, ',')
        .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(?<=\)|(?=\())|(;$)/)
        .filter(Boolean)
}

module.exports = splitCommandIntoArray
