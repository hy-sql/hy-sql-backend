const Joi = require('@hapi/joi')
const { ColumnSchema } = require('./FieldSchemas')

/**
 * Joi schema for validating DISTINCT keyword objects
 */
const DistinctSchema = Joi.object({
    type: Joi.string().valid('distinct').required(),
    value: Joi.array().items(ColumnSchema),
})

module.exports = DistinctSchema
