const queryContainsWhereKeyword = (fullCommandAsStringList) => {
    const where = fullCommandAsStringList.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )
    return where !== -1
}

/* expects as input a sliced version of the array containing the full command.
  The slicing must be done so that the input array begins with the expected
  location of the WHERE keyword. Method returns a command object to be validated
  as a part of the whole query.*/
const parseWhereToCommandObject = (slicedCommandAsStringList) => {
    const keyword = slicedCommandAsStringList[0]
    let index = 1

    let columnSignValue = slicedCommandAsStringList[1]
        ? slicedCommandAsStringList[1]
        : ''
    let signValueOption = slicedCommandAsStringList[2]
        ? slicedCommandAsStringList[2]
        : ''
    let valueOption = slicedCommandAsStringList[3]
        ? slicedCommandAsStringList[3]
        : ''

    let columnName = undefined
    let sign = findSign(columnSignValue)
    let valueType = 'INTEGER'
    let value = undefined
    let startQuotes = false
    let endQuotes = false

    let signSet = false

    if (!sign) {
        columnName = columnSignValue
        index++
        sign = findSign(signValueOption)

        if (sign) {
            sign =
                signValueOption.slice(0, sign.length) === sign
                    ? sign
                    : undefined
        }
    } else {
        columnName = columnSignValue.split(sign)[0]
        index++
        value =
            columnSignValue.endsWith(sign) ||
            columnSignValue.endsWith(`${sign}'`)
                ? signValueOption
                : columnSignValue.split(sign)[1]
        value === signValueOption ? index++ : ''
        startQuotes =
            value === signValueOption
                ? columnSignValue.endsWith("'") || value.startsWith("'")
                : value.startsWith("'")
        signSet = true
    }

    if (!sign) {
        value = signValueOption
        startQuotes = value.startsWith("'")
        index++
    } else if (sign && !signSet) {
        value = signValueOption.endsWith(sign)
            ? valueOption
            : signValueOption.split(sign)[1]
        index += value === valueOption ? 2 : 1
        startQuotes = value.startsWith("'")

        if (signValueOption.endsWith(sign) && valueOption === "'") {
            value = slicedCommandAsStringList[index]
            index++
        }
    }

    if (value === ';' && index >= slicedCommandAsStringList.length) {
        index--
        value = undefined
    }

    const unmodifiedValue = value
    if (value) {
        RegExp('^[0-9]+$').test(value)
            ? (value = Number(value))
            : (valueType = 'TEXT')

        valueType === 'TEXT' ? (value = value.replace(/'/g, '')) : ''
    }

    columnName === '' ? (columnName = undefined) : ''
    value === '' ? (value = undefined) : ''

    endQuotes =
        value && valueType === 'TEXT'
            ? unmodifiedValue.endsWith("'") ||
              slicedCommandAsStringList[index] === "'"
            : false

    valueType === 'TEXT' && slicedCommandAsStringList[index] === "'"
        ? index++
        : ''

    const correctQuotes =
        value && valueType === 'TEXT'
            ? startQuotes &&
              endQuotes &&
              !checkExtraQuotes(slicedCommandAsStringList, index, 2)
            : !checkExtraQuotes(slicedCommandAsStringList, index, 0)

    return {
        keyword,
        columnName,
        sign,
        valueType,
        value,
        indexCounter: index,
        correctQuotes,
    }
}

const findSign = (string) => {
    if (string.includes('<=')) {
        return '<='
    } else if (string.includes('>=')) {
        return '>='
    } else if (string.includes('=')) {
        return '='
    } else if (string.includes('<')) {
        return '<'
    } else if (string.includes('>')) {
        return '>'
    }

    return undefined
}

const checkExtraQuotes = (
    slicedCommandAsStringList,
    indexCounter,
    expectedQuotes
) => {
    const whereAsString = slicedCommandAsStringList
        .slice(0, indexCounter)
        .join('')
    const charactersOfWhere = [...whereAsString]
    const quotes = charactersOfWhere.filter((c) => c === "'").length

    return quotes > expectedQuotes
}

module.exports = { queryContainsWhereKeyword, parseWhereToCommandObject }
