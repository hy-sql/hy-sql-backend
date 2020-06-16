const { parseConditions } = require('./fieldParser')

const parseWhere = (slicedCommandAsStringArray) => {
    const indexOfOrder = slicedCommandAsStringArray.findIndex(
        (c) => c.toUpperCase() === 'ORDER'
    )

    const parsedWherePart = {
        keyword: slicedCommandAsStringArray[0],
        conditions: parseConditions(
            slicedCommandAsStringArray.slice(
                1,
                indexOfOrder >= 0
                    ? indexOfOrder
                    : slicedCommandAsStringArray.length
            )
        ),
    }

    return parsedWherePart
}

module.exports = { parseWhere }
