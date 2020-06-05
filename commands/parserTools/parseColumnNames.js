const { namify, cleanStringArray } = require('./smallTools')

const parseColumnNames = (stringArray, parserCounter) => {
    let laskuri = parserCounter
    let columnsOpenBrackets = 0
    const columns = []
    loop1: for (; laskuri < stringArray.length; laskuri++) {
        const element = stringArray[laskuri]
        switch (element.toUpperCase()) {
            case '(':
                columnsOpenBrackets++
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
                columns.push(element)
        }
    }

    if (laskuri !== stringArray.length) {
        return {
            pccolumns: laskuri,
            columns: namify(cleanStringArray(columns)),
        }
    }
    return { pccolumns: parserCounter }
}

module.exports = { parseColumnNames }
