const _ = require('lodash')

/**
 * Groups collection elements by multiple properties.
 * @param {Collection} collection
 * @param {Array} props
 * @returns Multiple nested arrays grouped by given properties
 */
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
