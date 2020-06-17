const Joi = require('@hapi/joi')

/**
 * Joi schema for validating the commandArray (array of sql queries) received in a HTTP request.
 */
const CommandArraySchema = Joi.array().items(Joi.string().min(1).required())

module.exports = CommandArraySchema
