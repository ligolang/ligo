import React, { Component } from "react";
import { Jumbotron, Container } from "reactstrap";

export default class Invalid extends Component {
  render() {
    const { fullName } = this.props.eosProject;
    return (
      <div className="custom-tab bg2">
        <Jumbotron style={{ background: "transparent" }} className="text-light">
          <Container>
            <h1>Invalid Project</h1>
            <hr className="my-2" />
            <p className="lead">
              Cannot read project settings for <kbd>{fullName}</kbd>.
            </p>
            <p className="lead">
              Make sure you have the read/write permission for <kbd>xxxx</kbd>.
            </p>
          </Container>
        </Jumbotron>
      </div>
    );
  }
}
