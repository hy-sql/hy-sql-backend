const supertest = require('supertest')
const app = require('../app')
const api = supertest(app)

test('API endpoint responds to ping', async (done) => {
    await api
        .get('/api/ping')
        .expect(200)
        .expect('Content-Type', /application\/json/)
    done()
})

test('API endpoint responds to query', async (done) => {
    const query = {
        commandArray: [
            'CREATE TABLE Tuotteet    (id INTEGER PRIMARY KEY,    nimi TEXT,    hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('retiisi', 7);",
        ],
    }

    await api
        .post('/api/query')
        .send(query)
        .expect(200)
        .expect('Content-Type', /application\/json/)
    done()
})

test('API endpoint responds sends 400 if query empty', async (done) => {
    const query = {}
    await api.post('/api/query').send(query).expect(400)
    done()
})
