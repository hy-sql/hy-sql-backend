const { parseField } = require('./fieldParser')

//TODO siivoa lopuksi ylimääräiset ja duplikaatit ja tiivistä
// Add check that correct type of fields for both thursday after discussing it. VALIDOINTIIN
// TODO JSDoc comments

const parseLimit = (slicedCommandAsStringArray) => {
    console.log(slicedCommandAsStringArray)

    const leftToParse = slicedCommandAsStringArray
    const limit = {}

    limit.keyword = leftToParse.splice(0, 1).toUpperCase()

    const indexOfOffset = leftToParse.findIndex(
        (s) => s.toUpperCase() === 'OFFSET'
    )

    if (indexOfOffset !== -1) {
        limit.offset = parseOffset(leftToParse.splice(indexOfOffset))
    }

    limit.field = parseField(leftToParse.join(''))

    return limit
}

const parseOffset = (slicedCommandAsStringArray) => {
    const offset = {
        keyword: slicedCommandAsStringArray[0].toUpperCase(),
        field: parseField(slicedCommandAsStringArray.slice(1).join('')),
    }

    return offset
}

module.exports = { parseLimit }
