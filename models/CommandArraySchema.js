const Joi = require('@hapi/joi')

const CommandArraySchema = Joi.array().items(Joi.string().min(1).required())

module.exports = CommandArraySchema
