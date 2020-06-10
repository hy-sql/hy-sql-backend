const { parseConditions } = require('./fieldParser')

const parseWhere = (slicedCommandAsStringArray) => {
    const indexOfOrderBy = slicedCommandAsStringArray.find(
        (s) => s.toUpperCase() === 'ORDER'
    )

    const parsedWherePart = {
        keyword: slicedCommandAsStringArray[0],
        conditions:
            indexOfOrderBy > 0
                ? parseConditions(
                      slicedCommandAsStringArray.slice(1, indexOfOrderBy)
                  )
                : parseConditions(
                      slicedCommandAsStringArray.slice(
                          1,
                          slicedCommandAsStringArray.length - 1
                      )
                  ),
    }

    return parsedWherePart
}

module.exports = { parseWhere }
