const queryContainsWhereKeyword = (fullCommandAsStringList) => {
    const where = fullCommandAsStringList.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )
    return where !== -1
}

module.exports = { queryContainsWhereKeyword }
