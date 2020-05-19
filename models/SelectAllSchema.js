const Joi = require('@hapi/joi')

// TODO: Add custom messages to errors and more rules
// Case-insensitivity in keywords?

const SelectAllSchema = Joi.object({
    name: Joi.string().required().valid('SELECT *').messages({}),

    from: Joi.string().required().valid('FROM').messages({}),

    tableName: Joi.string().required().alphanum().messages({}),

    finalSemicolon: Joi.string().required().valid(';').messages({}),
})

module.exports = SelectAllSchema
