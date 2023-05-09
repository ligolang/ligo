import React, { PureComponent } from "react";

import {
  ButtonGroup,
  Button,
  ButtonOptions,
  LoadingScreen,
  CenterScreen,
} from "~/base-components/ui-components";

import redux, { connect } from "~/base-components/redux";
import { actions } from "~/base-components/workspace";
import ProjectList from "./ProjectList";

class UserHomepage extends PureComponent {
  state = {
    notfound: false,
    loading: true,
    user: null,
    remote: true,
    projects: null,
  };

  constructor(props) {
    super(props);
    this.modal = React.createRef();
  }

  componentDidMount() {
    const { username } = this.props.match.params;
    this.setState({ remote: username !== "local" });
    this.getProjectList(username);
    this.state.remote && this.checkIsNewUser();
  }

  componentDidUpdate(props) {
    const { username } = this.props.match.params;
    const { username: prev } = props.match.params;
    if (username !== prev) {
      this.getProjectList(username);
    }
  }

  checkIsNewUser() {}

  getProjectList = async (username) => {
    if (username === "local") {
      this.setState({
        loading: false,
        notfound: false,
        user: null,
        projects: null,
      });
    }
  };

  isSelf = () => {
    const { profile, match } = this.props;
    return false;
  };

  renderCreateButton = () => {
    return (
      <Button color="warning" onClick={() => actions.newProject(this.state.remote)}>
        <i className="fas fa-plus mr-1" />
        New
      </Button>
    );
  };

  renderOpenButton = () => {
    return (
      <Button color="warning" className="border-left-gray" onClick={() => actions.openProject()}>
        <i className="fas fa-folder-plus mr-1" />
        Open
      </Button>
    );
  };

  renderProjectListOptions = () => {
    return (
      <ButtonGroup>
        <h4 color="primary">
          <i className="fas fa-th-list mr-2" />
          Projects
        </h4>
      </ButtonGroup>
    );
  };

  renderActionButtons = () => {
    if (!this.state.remote) {
      return (
        <ButtonGroup>
          {this.renderCreateButton()}
          {this.renderOpenButton()}
        </ButtonGroup>
      );
    }
    return this.renderCreateButton();
  };

  render() {
    const { profile, ProjectListItem } = this.props;
    const { loading, notfound, user, remote } = this.state;

    let projects;
    if (!remote) {
      projects = this.props.projects
        .get("local")
        .toJS()
        .map((p) => {
          delete p.author;
          return p;
        });
    } else {
      projects = this.state.projects;
    }

    if (!this.isSelf()) {
      if (loading) {
        return <LoadingScreen />;
      }
      if (notfound) {
        return (
          <CenterScreen>
            User <kbd>{user}</kbd> Not Found
          </CenterScreen>
        );
      }
    }

    return (
      <div className="d-flex w-100 h-100" style={{ overflow: "auto" }}>
        <div className="container py-5">
          <div className="d-flex flex-row justify-content-between my-3">
            {this.renderProjectListOptions()}
            {this.renderActionButtons()}
          </div>
          <ProjectList projects={projects} loading={loading} ListItem={ProjectListItem} />
        </div>
      </div>
    );
  }
}

export default connect(["profile", "projects"])(UserHomepage);
export { UserHomepage as BaseUserHomepage };

UserHomepage.propTypes = {};

UserHomepage.defaultProps = {};
