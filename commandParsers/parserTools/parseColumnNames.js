const { namify, cleanStringArray } = require('./smallTools')

/**
 * Parses column names from the given array while keeping count of current index.
 * Returns parsed column namess and the updated current index in an object or
 * an object containing only the updated current index if the index passes the
 * end of the given command array.
 * @param {string[]} stringArray command as string array
 * @param {Number} parserCounter current index
 */
const parseColumnNames = (stringArray, parserCounter) => {
    let counter = parserCounter
    let columnsOpenBrackets = 0
    let columns = []
    loop1: for (; counter < stringArray.length; counter++) {
        const element = stringArray[counter]
        switch (element.toUpperCase()) {
            case '(':
                columnsOpenBrackets++
                continue loop1
            case ',':
                continue loop1
            // Strings that break the loop, add more here: *TODO: RESERVED WORDS LIST*
            case ';':
            case 'SELECT':
            case 'CREATE':
            case 'UPDATE':
            case 'VALUES':
            case 'JOIN':
            case 'GROUP BY':
            case 'HAVING':
            case 'WHERE':
            case 'FROM':
                if (columnsOpenBrackets === 0) break loop1
                continue loop1
            case ')':
                if (columnsOpenBrackets > 0) {
                    columnsOpenBrackets--
                    continue loop1
                }
                break loop1
            default:
                if (columnsOpenBrackets > 0) continue loop1
                columns = columns.concat(element)
        }
    }

    if (counter !== stringArray.length) {
        return {
            pccolumns: counter,
            columns: namify(cleanStringArray(columns)),
        }
    }
    return { pccolumns: parserCounter }
}

module.exports = { parseColumnNames }
