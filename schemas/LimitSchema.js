const Joi = require('@hapi/joi')

// Add check that correct type of fields for both thursday after discussing it + custom message

// TODO LIMIT and OFFSET validation
// TODO JSDoc comments

const LimitSchema = Joi.object({})

const OffsetSchema = Joi.object({})

module.exports = { LimitSchema, OffsetSchema }
