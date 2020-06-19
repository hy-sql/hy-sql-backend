const { parseSelectFields } = require('./fieldParser')

const parseHaving = (slicedCommandAsStringArray) => {
    const parsedHavingPart =
        slicedCommandAsStringArray[0].toUpperCase() === 'HAVING'
            ? {
                  keyword: slicedCommandAsStringArray[0].toUpperCase(),
                  fields: parseSelectFields(
                      slicedCommandAsStringArray.slice(1)
                  ),
              }
            : null

    return parsedHavingPart
}

module.exports = { parseHaving }
