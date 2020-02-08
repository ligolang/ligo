import latestSchema from '../../src/schemas/share-latest';

describe('Latest Share Schema Migration', () => {
  it('should be v1', () => {
    expect(latestSchema.VERSION).toEqual('v1');
  });
});
