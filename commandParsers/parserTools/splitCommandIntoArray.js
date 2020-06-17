/**
 * Transforms the given command string into the format required by the command parsers.
 * Returns the command as string array.
 * @param {String} commandString the command/query as string
 */
const splitCommandIntoArray = (commandString) => {
    return commandString
        .trim()
        .replace(/\s\s+/g, ' ')
        .replace(/\s+,/g, ',')
        .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(?<=\)|(?=\())|(;$)/)
        .filter(Boolean)
}

module.exports = splitCommandIntoArray
