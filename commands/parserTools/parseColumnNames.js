const { namify, cleanStringArray } = require('./smallTools')

const parseColumnNames = (stringArray, parsedCommand) => {
    let laskuri = parsedCommand.parserCounter
    parsedCommand.columnsOpenBrackets = 0
    const columns = []
    loop1: for (; laskuri < stringArray.length; laskuri++) {
        const element = stringArray[laskuri]
        switch (element.toUpperCase()) {
            case '(':
                parsedCommand.columnsOpenBrackets++
                continue loop1
            case ',':
                continue loop1
            // katkaisevat sanat, lisää näitä, *TODO: RESERVED WORDS LISTA*
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
                if (parsedCommand.columnsOpenBrackets === 0) break loop1
                continue loop1
            case ')':
                if (parsedCommand.columnsOpenBrackets > 0) {
                    parsedCommand.columnsOpenBrackets--
                    continue loop1
                }
                break loop1
            default:
                if (parsedCommand.columnsOpenBrackets > 0) continue loop1
                columns.push(element)
        }
    }
    if (laskuri !== stringArray.length) {
        parsedCommand.parserCounter = laskuri
        parsedCommand.columns = namify(cleanStringArray(columns))
    }
    return parsedCommand
}

module.exports = { parseColumnNames }
