package cz.cvut.fel.ts1.refactoring;

import java.util.ArrayList;
import java.util.List;
import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class MailHelperTest {

    @Test
    public void createAndSendMail_ValidEmailParametersPassed_EmailSavedToDatabase()
    {
        TestableMailHelper helper = new TestableMailHelper();
        String to = "to@fel.cvut.cz";
        String subject = "Mail subject";
        String body = "body";
        Mail expectedMail = new Mail();
        expectedMail.setTo(to);
        expectedMail.setSubject(subject);
        expectedMail.setBody(body);
        expectedMail.setIsSent(true);

        helper.createAndSendMail(to, subject, body);

        assertTrue(helper.mailWasStored(expectedMail));
    }

    @Test
    public void createAndSendMail_ValidEmailParametersPassed_SendMailCalledExactlyOnce()
    {
        TestableMailHelper helper = new TestableMailHelper();
        String to = "to@fel.cvut.cz";
        String subject = "Mail subject";
        String body = "body";
        int expectedSendMailCallCount = 1;

        helper.createAndSendMail(to, subject, body);

        assertEquals(expectedSendMailCallCount, helper.getSendMailCallCounter());
    }

    static class TestableMailHelper extends MailHelper
    {
        private final List<Mail> mailStorage = new ArrayList<>();
        private int sendMailCallCounter = 0;

        public TestableMailHelper()
        {
            super(new DBManager());
        }

        @Override
        protected void insertMailToDb(Mail mail)
        {
            mailStorage.add(mail);
        }

        @Override
        protected void sendMailAsync(int mailId)
        {
            sendMail(mailId);
        }

        @Override
        protected Mail loadMail(int mailID)
        {
            for(Mail m : mailStorage)
            {
                if (m.getMailId() == mailID)
                {
                    return m;
                }
            }
            return null;
        }

        @Override
        protected void saveMailChanges(Mail mail)
        {
            Mail oldMail = loadMail(mail.getMailId());
            if (oldMail != null)
            {
                mailStorage.remove(mail);
            }
            mailStorage.add(mail);
        }

        @Override
        protected void sendMail(MimeMessage message) throws MessagingException
        {
            // DO NOTHING
            sendMailCallCounter++;
        }

        public boolean mailWasStored(Mail mail)
        {
            return mailStorage.contains(mail);
        }

        public int getSendMailCallCounter()
        {
            return sendMailCallCounter;
        }
    }
}
