import React, { PureComponent } from "react";

import { Link } from "react-router-dom";
import { DeleteButton, IconButton } from "~/base-components/ui-components";

import fileOps from "~/base-components/file-ops";
import { ProjectPath, actions } from "~/base-components/workspace";

export default class ProjectList extends PureComponent {
  removeProject = async (project) => {
    await actions.removeProject(project);
  };

  renameProject = async (project) => {
    await actions.renameProjects(project);
  };

  renderProjectRow = (project, index) => {
    return <tr key={`project-row-${index}`}>{this.renderProjectListItem(project)}</tr>;
  };

  renderProjectListItem = (project) => {
    const { ListItem } = this.props;
    if (ListItem) {
      return <ListItem project={project} />;
    }

    const { remote, author = "local", id, name, path } = project;
    const url = `/${author}/${id}`;

    return (
      <td className="d-flex flex-row justify-content-between hover-flex">
        <div className="flex-colume">
          <div className="mb-1 flex-row-center">
            <Link to={url} className="text-white">
              <h5 className="mb-0">{name}</h5>
            </Link>
          </div>
          <div className="mt-2 hover-off">
            <ProjectPath projectRoot={path} remote={remote} />
          </div>
        </div>
        <div className="d-flex flex-row align-items-start hover-show">
          {this.renderRightButton(project)}
        </div>
      </td>
    );
  };

  renderRightButton = (project) => {
    if (!project.remote) {
      return (
        <div>
          <DeleteButton
            textConfirm="Click again to remove"
            onConfirm={() => this.removeProject(project)}
          />
          <IconButton
            color="transparent"
            id={`confirm-delete-${project.id}`}
            className="text-muted"
            icon="fas fa-edit"
            onClick={() => this.renameProject(project)}
          />
        </div>
      );
    }

    return null;
  };

  render() {
    const { loading, projects } = this.props;

    if (loading && !projects) {
      return (
        <table className="table table-hover table-striped">
          <tbody>
            <tr key="no-project">
              <td key="loading" align="middle" className="text-muted">
                <i className="fas fa-spin fa-spinner mr-1" />
                Loading...
              </td>
            </tr>
          </tbody>
        </table>
      );
    }

    if (!projects || !projects.length) {
      return (
        <table className="table table-hover table-striped">
          <tbody>
            <tr key="no-project">
              <td align="middle" className="text-muted">
                (No Project)
              </td>
            </tr>
          </tbody>
        </table>
      );
    }

    return (
      <table className="table table-hover table-striped">
        <tbody>{projects.map(this.renderProjectRow)}</tbody>
      </table>
    );
  }
}
