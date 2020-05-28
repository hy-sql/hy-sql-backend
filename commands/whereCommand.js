const queryContainsWhereKeyword = (fullCommandAsStringList) => {
    const where = fullCommandAsStringList.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )
    console.log('has where: ', where > 0)
    return where > 0
}

/* expects as input a sliced version of the array containing the full command.
  The slicing must be done so that the input array begins with the expected
  location of the WHERE keyword. Method returns a command object to be validated
  as a part of the whole query.*/
const parseWhereToCommandObject = (slicedCommandAsStringList) => {
    const keyword = slicedCommandAsStringList[0]
    let columnSignValue = slicedCommandAsStringList[1]
        ? slicedCommandAsStringList[1]
        : ''

    let sign
    if (columnSignValue.includes('<=')) {
        sign = '<='
    } else if (columnSignValue.includes('>=')) {
        sign = '>='
    } else if (columnSignValue.includes('=')) {
        sign = '='
    } else if (columnSignValue.includes('<')) {
        sign = '<'
    } else if (columnSignValue.includes('>')) {
        sign = '>'
    }

    const columnName = sign ? columnSignValue.split(sign)[0] : columnSignValue
    let value = sign ? columnSignValue.split(sign)[1] : undefined

    let valueType = 'INTEGER'
    if (value) {
        RegExp('^[0-9]+$').test(value)
            ? (value = Number(value))
            : (valueType = 'TEXT')
    }

    return {
        keyword,
        columnName,
        sign,
        valueType,
        value,
    }
}

module.exports = { queryContainsWhereKeyword, parseWhereToCommandObject }
