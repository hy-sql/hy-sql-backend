const { namify, cleanStringArray } = require('./smallTools')

const parseColumnNames = (parserCounter, fullCommandAsStringList) => {
    let laskuri = parserCounter
    const returnable = { parserCounter, columnsOpenBrackets: 0 }
    const columns = []
    loop1: for (; laskuri < fullCommandAsStringList.length; laskuri++) {
        const element = fullCommandAsStringList[laskuri]
        switch (element.toUpperCase()) {
            case '(':
                returnable.columnsOpenBrackets++
                continue loop1
            case ',':
                continue loop1
            // katkaisevat sanat, lisää näitä
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
                if (returnable.columnsOpenBrackets === 0) break loop1
                continue loop1
            case ')':
                if (returnable.columnsOpenBrackets > 0) {
                    returnable.columnsOpenBrackets--
                    continue loop1
                }
                break loop1
            default:
                if (returnable.columnsOpenBrackets > 0) continue loop1
                columns.push(element)
        }
    }
    if (laskuri !== fullCommandAsStringList.length) {
        returnable.parserCounter = laskuri
        returnable.columns = namify(cleanStringArray(columns))
    }
    return returnable
}

module.exports = { parseColumnNames }
