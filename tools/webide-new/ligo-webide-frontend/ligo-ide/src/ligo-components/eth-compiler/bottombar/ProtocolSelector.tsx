/* eslint-disable @typescript-eslint/ban-ts-comment */
/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import { useSelector, useDispatch } from "react-redux";
// @ts-ignore
import Config from "Config";
import {
  UncontrolledButtonDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
} from "~/base-components/ui-components";

const ProtocolSelector = () => {
  // @ts-ignore
  const protocol: { showName: string; name: string } = useSelector((state) => state.protocol);

  const dispatch = useDispatch();

  const {
    protocols,
  }: {
    protocols: {
      showName: string;
      name: string;
    }[];
  } = Config;

  const menuHeader = (
    <>
      <i className="fas fa-clipboard-list mr-1" />
      Protocols
    </>
  );
  return (
    <UncontrolledButtonDropdown direction="up">
      <DropdownToggle
        size="sm"
        color="default"
        className="rounded-0 text-muted px-2 text-nowrap overflow-hidden text-overflow-dots"
        style={{ maxWidth: 300 }}
      >
        <span key="icon" className="mr-1">
          <i className="fas fa-clipboard-list" />
        </span>
        {`Protocol ${protocol.showName}`}
      </DropdownToggle>
      <DropdownMenu right className="dropdown-menu-sm">
        <DropdownItem header>{menuHeader}</DropdownItem>
        {protocols.map((pr) => (
          <DropdownItem
            key={`image-version-${pr.name}`}
            active={protocol.name === pr.name}
            onClick={() => dispatch({ type: "SET_PROTOCOL", payload: pr })}
          >
            {pr.showName}
          </DropdownItem>
        ))}
      </DropdownMenu>
    </UncontrolledButtonDropdown>
  );
};

export default ProtocolSelector;
