const { InsertIntoSchema } = require('../models/InsertIntoSchema')

const parseCommand = (fullCommandAsStringList) => {
    let anchorLocation = fullCommandAsStringList
        .join(' ')
        .toUpperCase()
        .split(' ')
        .indexOf('VALUES')

    if (anchorLocation === -1) {
        return {
            value: { name: fullCommandAsStringList.slice(0, 2).join(' ') },
            error: {
                details: [
                    {
                        message:
                            'INSERT INTO needs a VALUES keyword before the actual values to be inserted',
                    },
                ],
            },
        }
    }

    const columnList = cleanStringArray(
        fullCommandAsStringList.slice(
            fullCommandAsStringList[3] === '(' ? 4 : 3,
            fullCommandAsStringList[anchorLocation - 1] === ')'
                ? anchorLocation - 1
                : anchorLocation
        )
    ).map((col) => {
        return { name: col }
    })

    /*
    for (let index = anchorLocation + 1; index < array.length; index++) {
        const element = array[index];

    }
        const values = addAttributesToValuesArray(
            columnList,
            cleanStringArray(
                fullCommandAsStringList.slice(
                    fullCommandAsStringList[anchorLocation + 1] &&
                        fullCommandAsStringList[anchorLocation + 1] === '('
                        ? anchorLocation + 2
                        : anchorLocation + 1,
                    fullCommandAsStringList.length - 2
                )
            )
        )
        */

    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        columnsOpeningBracket:
            fullCommandAsStringList[3] === '(' ? '(' : undefined,
        columns: columnList,
        columnsClosingBracket:
            fullCommandAsStringList[anchorLocation - 1] === ')'
                ? ')'
                : undefined,
        anchorKeyword: fullCommandAsStringList[anchorLocation],
        valuesOpeningBracket:
            fullCommandAsStringList[anchorLocation + 1] &&
            fullCommandAsStringList[anchorLocation + 1] === '('
                ? '('
                : undefined,
        valuesClosingBracket:
            fullCommandAsStringList[fullCommandAsStringList.length - 2] === ')'
                ? ')'
                : undefined,
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    //VALUES kentät
    const parseErrors = []
    let avoimetSulut = 0
    const valueTaulukko = []
    let lohko = []
    // TODO pari rajatapausta, katso kohta
    for (
        let index = anchorLocation;
        index < fullCommandAsStringList.length;
        index++
    ) {
        const element = fullCommandAsStringList[index]
        if (element === ';') {
            if (lohko.length !== 0) {
                valueTaulukko.push(cleanStringArray(lohko))
            }
            break
        }
        if (element === '(') {
            if (avoimetSulut > 0) {
                parseErrors.push({
                    message: 'ylimääräinen avaava sulku value lohkossa',
                })
                continue
            } else {
                avoimetSulut++
                continue
            }
        }
        if (element === ')' || element === '),') {
            if (avoimetSulut > 0) {
                avoimetSulut = 0
                valueTaulukko.push(
                    addAttributesToValuesArray(
                        parsedCommand.columns,
                        cleanStringArray(lohko)
                    )
                )
                console.log(valueTaulukko)
                lohko = []
                continue
            } else {
                parseErrors.push({
                    message: 'ylimääräinen sulkeva sulku value lohkossa',
                })
                continue
            }
        }
        if (element === ')(') {
            parseErrors.push({
                message: 'eri arvojoukkojen välissä on oltava pilkku',
            })
            valueTaulukko.push(
                addAttributesToValuesArray(
                    parsedCommand.columns,
                    cleanStringArray(lohko)
                )
            )
            lohko = []
            continue
        }
        if (element === ',') {
            console.log(element)
            continue
        }
        lohko.push(element)
    }
    if (avoimetSulut > 0) {
        parseErrors.push({ message: 'values lohkosta puuttuu sulkeva sulku' })
    }
    parsedCommand.values = valueTaulukko

    if (parsedCommand.error) return parsedCommand.error

    return InsertIntoSchema.validate(parsedCommand)
}

const cleanStringArray = (columnsAsStringList) => {
    return columnsAsStringList
        .join(' ')
        .split(', ')
        .map((col) => col.trim())
}

const addAttributesToValuesArray = (columnList, stringArray) => {
    const taulukko = stringArray.map((value, index) =>
        value.match('[0-9]')
            ? {
                  column: columnList[index] ? columnList[index].name : null,
                  value,
                  type: 'INTEGER',
              }
            : {
                  column: columnList[index] ? columnList[index].name : null,
                  value: value.replace(/'/g, ' ').trim(),
                  type: 'TEXT',
              }
    )
    return taulukko
}

module.exports = { parseCommand }
