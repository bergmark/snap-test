<html>
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
  </head>
  <body>
    <div id="content">
      <h1>It works!</h1>
      <p>
        Echo test:
        <a href="/echo/cats">cats</a>
        <a href="/echo/dogs">dogs</a>
        <a href="/echo/fish">fish</a>
      </p>
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
