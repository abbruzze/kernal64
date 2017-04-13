package ucesoft.cbm.util

import javax.swing.JFrame
import javax.swing.JDialog
import com.dropbox.core.DbxAppInfo
import java.util.Properties
import java.util.Locale
import com.dropbox.core.DbxRequestConfig
import com.dropbox.core.DbxWebAuthNoRedirect
import javax.swing.JOptionPane
import com.dropbox.core.DbxAuthInfo
import com.dropbox.core.DbxHost
import com.dropbox.core.DbxClient
import java.awt.event.ActionListener
import javax.swing.JPanel
import java.awt.Desktop
import javax.swing.JButton
import javax.swing.JLabel
import javax.swing.BoxLayout
import java.awt.event.ActionEvent
import java.net.URI
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection

object DropBoxAuth {
  private[this] val KEY = "in4xusy0av58oje"
  private[this] val SECRET = "zvbpznfdjxppdyx"
  private[this] val APP_INFO = new DbxAppInfo(KEY,SECRET)
  private[this] val ACCESS_CODE_PROP_KEY = "DROPBOX_ACCESCODE"
  
  private class AuthPanel(url:String) extends JPanel with ActionListener {
    val button = new JButton("Open browser for Dropbox authentication")
    val label = new JLabel(s"<html><a href='$url'>$url</a></html>")
    
    setLayout(new BoxLayout(this,BoxLayout.Y_AXIS))
    if (Desktop.isDesktopSupported) {
      add(new JLabel("<html>Dropbox needs you to authenicate.<br>Push the button to open the browser</html>"))
      add(button)      
    }
    else {
      Toolkit.getDefaultToolkit.getSystemClipboard.setContents(new StringSelection(url),null)
      add(new JLabel("Paste in your browser the Dropbox URL copied in the clipboard and follow the authentication process"))
    }
    add(new JLabel("Then, copy the authorization code below"))
    button.addActionListener(this)
    
    def actionPerformed(e:ActionEvent) {
      Desktop.getDesktop.browse(new URI(url))
      button.setEnabled(false)
    }
  }
  
  def requestAuthorization(prop:Properties,parentFrame:JFrame) : Boolean = {
    try {
      val requestConfig = new DbxRequestConfig("Kernal64-authorization", Locale.getDefault.toString)
      val webAuth = new DbxWebAuthNoRedirect(requestConfig, APP_INFO)
      val authorizeUrl = webAuth.start
      
      val authCode = JOptionPane.showInputDialog(parentFrame,new AuthPanel(authorizeUrl),"Dropbox authorization request", JOptionPane.QUESTION_MESSAGE)
      if (authCode != null) {
        val authFinish = webAuth.finish(authCode.trim)   
        prop.setProperty(ACCESS_CODE_PROP_KEY,authFinish.accessToken)
        true
      }
      else false
    }
    catch {
      case t:Throwable =>
        JOptionPane.showMessageDialog(parentFrame,"Error while authenticating to Dropbox: " + t.getMessage,"Dropxbox error",JOptionPane.ERROR_MESSAGE)
        false
    }
  }
  
  def isAccessCodeRequested(prop:Properties) = prop.getProperty(ACCESS_CODE_PROP_KEY) != null
  
  def getDbxClient(prop:Properties) : DbxClient = {
    val accessCode = prop.getProperty(ACCESS_CODE_PROP_KEY)
    if (accessCode == null) throw new IllegalArgumentException("Can't find Dropbox access code in the configuration file")
    
    val authInfo = new DbxAuthInfo(accessCode,DbxHost.Default)
    val requestConfig = new DbxRequestConfig("Kernal64-client", Locale.getDefault.toString)
    new DbxClient(requestConfig, authInfo.accessToken, authInfo.host)
  }
}