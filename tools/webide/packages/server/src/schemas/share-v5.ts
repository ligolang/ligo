import joi from '@hapi/joi';

import { Migration } from './migration';
import { SchemaMigrationV4, SchemaV4 } from './share-v4';

export type Version = 'v5';

export interface SchemaV5 {
  version: Version;
  state: {
    editor: {
      language: string;
      code: string;
      dirty: boolean;
      title: string;
    };
    compile: {
      entrypoint: string;
      protocol: string;
    };
    dryRun: {
      entrypoint: string;
      parameters: string;
      storage: string;
      protocol: string;
    };
    deploy: {
      entrypoint: string;
      storage: string;
      network: string;
      signer: string;
      protocol: string;
    };
    evaluateValue: {
      entrypoint: string;
      protocol: string;
    };
    evaluateFunction: {
      entrypoint: string;
      parameters: string;
      protocol: string;
    };
    generateDeployScript: {
      tool: string;
      entrypoint: string;
      storage: string;
      originationAccount: string;
      burnCap: number;
      protocol: string;
    };
  };
}

export class SchemaMigrationV5 extends Migration {
  readonly VERSION: Version = 'v5';

  protected readonly schema = joi.object({
    version: joi.string().required().allow(this.VERSION),
    state: joi.object({
      editor: joi
        .object({
          language: joi.string().required(),
          code: joi.string().required(),
          dirty: joi.boolean().optional(),
          title: joi.string().allow(''),
        })
        .required(),
      compile: joi.object({
        entrypoint: joi.string().allow(''),
        protocol: joi.string().required(),
      }),
      dryRun: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow(''),
        storage: joi.any().allow(''),
        protocol: joi.string().required(),
      }),
      deploy: joi.object({
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        network: joi.string().allow(''),
        signer: joi.string().allow(''),
        protocol: joi.string().required(),
      }),
      evaluateValue: joi.object({
        entrypoint: joi.string().allow(''),
        protocol: joi.string().required(),
      }),
      evaluateFunction: joi.object({
        entrypoint: joi.string().allow(''),
        parameters: joi.any().allow(''),
        protocol: joi.string().required(),
      }),
      generateDeployScript: joi.object({
        tool: joi.string().allow(''),
        entrypoint: joi.string().allow(''),
        storage: joi.any().allow(''),
        originationAccount: joi.string().allow(''),
        burnCap: joi.number().allow(''),
        protocol: joi.string().required(),
      }),
    }),
  });

  protected readonly previous = new SchemaMigrationV4();

  protected migrate(data: SchemaV4): SchemaV5 {
    return {
      ...data,
      version: this.VERSION,
      state: {
        ...data.state,
        compile: {
          ...data.state.compile,
          protocol: '',
        },
        dryRun: {
          ...data.state.dryRun,
          protocol: '',
        },
        deploy: {
          ...data.state.deploy,
          protocol: '',
        },
        evaluateValue: {
          ...data.state.evaluateValue,
          protocol: '',
        },
        evaluateFunction: {
          ...data.state.evaluateFunction,
          protocol: '',
        },
        generateDeployScript: {
          ...data.state.generateDeployScript,
          protocol: '',
        },
      },
    };
  }
}
