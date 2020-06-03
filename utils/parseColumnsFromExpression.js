const _ = require('lodash')

const parseColumnsFromExpression = (fieldValue) =>
    _.words(fieldValue).filter((w) => isNaN(w))

module.exports = { parseColumnsFromExpression }
