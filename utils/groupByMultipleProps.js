const _ = require('lodash')

const groupByMultipleProps = (collection, props) => {
    if (!props.length) return collection

    const groupedBy = _.chain(collection)
        .groupBy(props[0])
        .mapValues((values) => groupByMultipleProps(values, props.slice(1)))
        .values()
        .value()

    return groupedBy
}

module.exports = groupByMultipleProps
