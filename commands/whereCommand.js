const queryContainsWhereKeyword = (fullCommandAsStringList) => {
    const where = fullCommandAsStringList.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )
    return where > 0
}

/* expects as input a sliced version of the array containing the full command.
  The slicing must be done so that the input array begins with the expected
  location of the WHERE keyword. Method returns a command object to be validated
  as a part of the whole query.*/
const parseWhereToCommandObject = (slicedCommandAsStringList) => {
    const keyword = slicedCommandAsStringList[0]

    let index = 1
    slicedCommandAsStringList[index] === "'" ? index++ : ''
    let columnSignValue = slicedCommandAsStringList[index]
        ? slicedCommandAsStringList[index]
        : ''
    index++
    let signValueOption = slicedCommandAsStringList[index]
        ? slicedCommandAsStringList[index]
        : ''
    index++
    let valueOption = slicedCommandAsStringList[index]
        ? slicedCommandAsStringList[index]
        : ''
    index++

    let columnName = undefined
    let sign = findSign(columnSignValue)
    let valueType = 'INTEGER'
    let value = undefined

    let signSet = false

    if (!sign) {
        columnName = columnSignValue
        sign = findSign(signValueOption)

        if (sign) {
            sign =
                signValueOption.slice(0, sign.length) === sign
                    ? sign
                    : undefined
        }
    } else {
        columnName = columnSignValue.split(sign)[0]
        value =
            columnSignValue.endsWith(sign) ||
            columnSignValue.endsWith(`${sign}'`)
                ? signValueOption
                : columnSignValue.split(sign)[1]
        signSet = true
    }

    if (!sign) {
        value = signValueOption
    } else if (sign && !signSet) {
        value = signValueOption.endsWith(sign)
            ? valueOption
            : signValueOption.split(sign)[1]

        if (signValueOption.endsWith(sign) && valueOption === "'") {
            value = slicedCommandAsStringList[index]
            index++
        }
    }

    if (value === ';' && index >= slicedCommandAsStringList.length) {
        index--
        value = undefined
    }

    if (value) {
        RegExp('^[0-9]+$').test(value)
            ? (value = Number(value))
            : (valueType = 'TEXT')

        valueType === 'TEXT' ? (value = value.replace(/'/g, '')) : ''
    }

    columnName === '' ? (columnName = undefined) : ''
    value === '' ? (value = undefined) : ''

    slicedCommandAsStringList[index + 1] === "'" ? index++ : ''

    return {
        keyword,
        columnName,
        sign,
        valueType,
        value,
        indexCounter: index,
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

module.exports = { queryContainsWhereKeyword, parseWhereToCommandObject }
