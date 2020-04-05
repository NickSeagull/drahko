/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

class HomeSplash extends React.Component {
  render() {
    const { siteConfig, language = "" } = this.props;
    const { baseUrl, docsUrl } = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ""}`;
    const langPart = `${language ? `${language}/` : ""}`;
    const docUrl = (doc) => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SplashContainer = (props) => (
      <div className="homeContainer">
        <div className="homeSplashFade">
          <div className="wrapper homeWrapper">{props.children}</div>
        </div>
      </div>
    );

    const Logo = (props) => (
      <div className="projectLogo">
        <img src={props.img_src} alt="Project Logo" />
      </div>
    );

    const ProjectTitle = (props) => (
      <h2 className="projectTitle">
        {props.title}
        <small>{props.tagline}</small>
      </h2>
    );

    const PromoSection = (props) => (
      <div className="section promoSection">
        <div className="promoRow">
          <div className="pluginRowBlock">{props.children}</div>
        </div>
      </div>
    );

    const Button = (props) => (
      <div className="pluginWrapper buttonWrapper">
        <a className="button" href={props.href} target={props.target}>
          {props.children}
        </a>
      </div>
    );

    return (
      <SplashContainer>
        <div className="inner">
          <h1>
            <img src={`${baseUrl}img/logo.png`}></img>
          </h1>
          <h2>{siteConfig.tagline}</h2>
          <PromoSection>
            <Button href={docUrl("doc-install")}>Get started</Button>
            <Button href="https://discord.gg/dtqJ6PC">Discord</Button>
            <Button href="https://github.com/drahko/drahko">GitHub</Button>
          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

class Index extends React.Component {
  render() {
    const { config: siteConfig, language = "" } = this.props;
    const { baseUrl } = siteConfig;

    const Block = (props) => (
      <Container
        padding={["bottom", "top"]}
        id={props.id}
        background={props.background}
      >
        <GridBlock
          align={props.align ?? "left"}
          contents={props.children}
          layout={props.layout}
        />
      </Container>
    );

    const FeatureCallout = () => (
      <div
        className="productShowcaseSection paddingBottom"
        style={{ textAlign: "center" }}
      >
        <h2>Feature Callout</h2>
        <MarkdownBlock>These are features of this project</MarkdownBlock>
      </div>
    );

    const TryOut = () => (
      <Block id="try">
        {[
          {
            content:
              "To make your landing page more attractive, use illustrations! Check out " +
              "[**unDraw**](https://undraw.co/) which provides you with customizable illustrations which are free to use. " +
              "The illustrations you see on this page are from unDraw.",
            image: `${baseUrl}img/undraw_code_review.svg`,
            imageAlign: "left",
            title: "Wonderful SVG Illustrations",
          },
        ]}
      </Block>
    );

    const Description = () => (
      <div class="custom-description">
        <Block>
          {[
            {
              title: "What is Drahko?",
              content: `Drahko is a desktop automation tool for Windows. It allows you to automate applications and websites by writing **Drahko scripts**.
You can automate any workflow you want, be it simple as pressing keys in sequence, or complex like retrieving
data from the internet, transforming it, and saving it into another place.
              `,
              image: `${baseUrl}img/undraw_online_test.svg`,
              imageAlign: "right",
            },
          ]}
        </Block>
        <Block>
          {[
            {
              title: "Automate any task",
              content: `
If you can perform it manually, you can automate it with Drahko. Typing your full name, opening Twitter, controlling Spotify,
creating reports, filtering the clipboard...

Drahko comes with a lot of powerful features:

* Key bindings
* Creation of user interfaces
* Access to file system
* Many more!
              `,
              image: `${baseUrl}img/undraw_dev_focus.svg`,
              imageAlign: "left",
            },
          ]}
        </Block>
        <Block>
          {[
            {
              title: "Based on the best technologies",
              content: `
Drahko is based on the battle-tested [AutoHotkey](https://www.autohotkey.com/) software, which has been used for many years by thousands of users to automate
their computing experience.

At it's core, Drahko connects AutoHotkey with [Idris](https://www.idris-lang.org/), a cutting-edge programming language that removes most of the common errors
programmers face daily. Add some nice utilities and sugar on top of that, and you have the ultimate automation experience for Windows.
              `,
              image: `${baseUrl}img/undraw_lost_online.svg`,
              imageAlign: "right",
            },
          ]}
        </Block>
        <Block>
          {[
            {
              title: "Open Source",
              content: `
Drahko is 100% open source and **free**, which means that **you** have the control of the software you are running on your computer.
This enables collaboration between developers around the world. Everyone has voice in this project, and can request and create features, submit
patches to fix bugs, and even help with the documentation website that you are reading right now.
              `,
              image: `${baseUrl}img/undraw_open_source.svg`,
              imageAlign: "left",
            },
          ]}
        </Block>
      </div>
    );

    const LearnHow = () => (
      <Block align="center" background="light">
        {[
          {
            title: "Get started now!",
            content: `Why don't you give Drahko a try? It will always be free, with no strings attached.

Drahko is the ultimate automation tool for your
PC. Start using it and see it for yourself!

[Go to documentation](${siteConfig.docsUrl}/doc-install)`,
            imageAlign: "top",
          },
        ]}
      </Block>
    );

    const Features = () => (
      <div>
        <Block align="center">
          {[
            {
              title: "[Join us on Discord](https://discord.gg/dtqJ6PC)",
              content: "",
              image: `${baseUrl}img/discord-icon.svg`,
              imageAlign: "top",
            },
            {
              title:
                "[Follow the development on GitHub](https://github.com/drahko/drahko)",
              content: "",
              image: `${baseUrl}img/github-icon.svg`,
              imageAlign: "top",
            },
          ]}
        </Block>
      </div>
    );

    const Showcase = () => {
      if ((siteConfig.users || []).length === 0) {
        return null;
      }

      const showcase = siteConfig.users
        .filter((user) => user.pinned)
        .map((user) => (
          <a href={user.infoLink} key={user.infoLink}>
            <img src={user.image} alt={user.caption} title={user.caption} />
          </a>
        ));

      const pageUrl = (page) =>
        baseUrl + (language ? `${language}/` : "") + page;

      return (
        <div className="productShowcaseSection paddingBottom">
          <h2>Who is Using This?</h2>
          <p>This project is used by all these people</p>
          <div className="logos">{showcase}</div>
          <div className="more-users">
            <a className="button" href={pageUrl("users.html")}>
              More {siteConfig.title} Users
            </a>
          </div>
        </div>
      );
    };

    return (
      <div>
        <HomeSplash siteConfig={siteConfig} language={language} />
        <div className="mainContainer">
          <Description />
          <LearnHow />
          <Features />
        </div>
      </div>
    );
  }
}

module.exports = Index;
