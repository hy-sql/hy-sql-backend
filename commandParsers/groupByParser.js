const splitCommandIntoArray = require('./parserTools/splitCommandIntoArray')

const { parseSelectFields } = require('./fieldParser')

const parseGroupBy = (slicedCommandAsStringArray) => {
    const parsedGroupByPart =
        slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'GROUP BY'
            ? {
                  keyword: slicedCommandAsStringArray
                      .slice(0, 2)
                      .join(' ')
                      .toUpperCase(),
                  columns: parseSelectFields(
                      slicedCommandAsStringArray.slice(2)
                  ),
              }
            : null

    return parsedGroupByPart
}

module.exports = { parseGroupBy }

const query = 'GROUP BY nimi, hinta'

const splitQuery = splitCommandIntoArray(query)

console.log(splitQuery)

console.log(parseGroupBy(splitQuery))
