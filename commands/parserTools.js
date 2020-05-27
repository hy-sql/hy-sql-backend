const cleanStringArray = (stringArray) => {
    return stringArray
        .join(' ')
        .split(', ')
        .map((col) => col.trim())
}

const namify = (stringList) => {
    return stringList.map((str) => {
        return {
            name: str,
        }
    })
}

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
            //lista? katsotaan löytyykö sana listasta? en tiedä vielä miten sen tekis casen kanssa
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
    console.log(returnable)
    return returnable
}

const addAttributesToValuesArray = (columns, stringArray) => {
    const taulukko = stringArray.map((value, index) =>
        value.match('[0-9]')
            ? {
                  column: columns[index] ? columns[index].name : null,
                  value,
                  type: 'INTEGER',
              }
            : {
                  column: columns[index] ? columns[index].name : null,
                  value: value.replace(/'/g, ' ').trim(),
                  type: 'TEXT',
              }
    )
    return taulukko
}

module.exports = {
    cleanStringArray,
    addAttributesToValuesArray,
    parseColumnNames,
}
