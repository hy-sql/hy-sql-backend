const Joi = require('@hapi/joi')
const WhereSchema = require('./WhereSchema')
const GroupBySchema = require('./GroupBySchema')
const HavingSchema = require('./HavingSchema')
const OrderBySchema = require('./OrderBySchema')
const {
    AllFieldsSchema,
    ColumnSchema,
    TextSchema,
    IntegerSchema,
} = require('./FieldSchemas')
const { ExpressionSchema } = require('./ExpressionSchema')
const DistinctSchema = require('./DistinctSchema')
const FunctionSchema = require('./FunctionSchema')
const LimitSchema = require('./LimitSchema')

/**
 * Joi schema for validating SELECT commands not containing WHERE or ORDER BY.
 */
const SelectSchema = Joi.object({
    name: Joi.string().required().valid('SELECT').insensitive().messages({
        'any.only': 'Query must begin with SELECT *',
        'any.required': 'Query must begin with SELECT *',
    }),

    fields: Joi.array()
        .items(
            AllFieldsSchema,
            ColumnSchema,
            TextSchema,
            IntegerSchema,
            ExpressionSchema.shared(FunctionSchema),
            FunctionSchema.shared(ExpressionSchema),
            DistinctSchema
        )
        .min(1)
        .required(),

    from: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^FROM$/i)
        .messages({
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base': 'SELECT * must be followed by FROM',
            'any.required': 'SELECT * must be followed by FROM',
        }),

    tableName: Joi.string().required().pattern(/^\w+$/).max(64).messages({
        'any.required': 'Query must contain a table name',
        'string.pattern.base':
            'Table name should only contain one or more alphanumeric characters and underscores',
        'string.max': 'The table name is too long',
    }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),

    limit: LimitSchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE but not ORDER BY.
 */
const SelectWhereSchema = SelectSchema.keys({
    where: WhereSchema,
})

const SelectGroupBySchema = SelectSchema.keys({
    groupBy: GroupBySchema,
})

const SelectGroupByHavingSchema = SelectSchema.keys({
    groupBy: GroupBySchema,
    having: HavingSchema,
})

/**
 * Joi schema for validating SELECT commands containing ORDER BY but not WHERE.
 */
const SelectOrderBySchema = SelectSchema.keys({
    orderBy: OrderBySchema,
})

const SelectWhereGroupBySchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
})

const SelectWhereGroupByHavingSchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
    having: HavingSchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE and ORDER BY.
 */
const SelectWhereOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    orderBy: OrderBySchema,
})

const SelectGroupByOrderBySchema = SelectSchema.keys({
    groupBy: GroupBySchema,
    orderBy: OrderBySchema,
})

const SelectGroupByHavingOrderBySchema = SelectSchema.keys({
    groupBy: GroupBySchema,
    having: HavingSchema,
    orderBy: OrderBySchema,
})

const SelectWhereGroupByOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
    orderBy: OrderBySchema,
})

const SelectWhereGroupByHavingOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
    having: HavingSchema,
    orderBy: OrderBySchema,
})

module.exports = {
    SelectSchema,
    SelectWhereSchema,
    SelectGroupBySchema,
    SelectGroupByHavingSchema,
    SelectOrderBySchema,
    SelectWhereGroupBySchema,
    SelectWhereGroupByHavingSchema,
    SelectWhereOrderBySchema,
    SelectGroupByOrderBySchema,
    SelectGroupByHavingOrderBySchema,
    SelectWhereGroupByOrderBySchema,
    SelectWhereGroupByHavingOrderBySchema,
}
