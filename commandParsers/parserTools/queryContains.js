const queryContainsWhereKeyword = (fullCommandAsStringArray) => {
    const where = fullCommandAsStringArray.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )
    return where !== -1
}

module.exports = { queryContainsWhereKeyword }
