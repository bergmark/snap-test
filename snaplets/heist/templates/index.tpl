<html>
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
  </head>
  <body>
    <div id="content">
      <h1>Snap test!</h1>

      <ifLoggedIn>
        <p>
          This is a simple demo page served using
          <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
          and the <a href="http://snapframework.com/">Snap</a> web framework.
        </p>

        <p>Congrats!  You're logged in as '<loggedInUser/>'</p>

        <p><a href="/logout">Logout</a></p>
      </ifLoggedIn>

      <ifLoggedOut>
        <apply template="_login"/>
      </ifLoggedOut>

      <table id="info">
        <tr>
          <td>Config generated at:</td>
          <td><start-time/></td>
        </tr>
        <tr>
          <td>Page generated at:</td>
          <td><current-time/></td>
        </tr>
        <tr>
          <td>Last posted message:</td>
          <td><message/></td>
        </tr>
        <tr>
          <td>Edit message:</td>
          <td><message-form/></td>
        </tr>
        <tr>
          <td>Session info:</td>
          <td><session-info/></td>
        </tr>
        <tr>
          <td>GET foo</td>
          <td><get-foo/></td>
        </tr>
      </table>
    </div>
  </body>
</html>
