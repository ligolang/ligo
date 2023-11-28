import { LigoContext } from './common/LigoContext';
import { LigoProtocolClient } from './common/LigoProtocolClient';

import LigoServer from './debugger/LigoServer';

export abstract class LigoExtension {
  /* These methods should be called exactly once in extension.ts */
  public abstract activate(context: LigoContext, server: LigoServer, client: LigoProtocolClient): void;
  public abstract deactivate(): Thenable<void> | void | undefined;
}
