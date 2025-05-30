package cz.cvut.fel.omo.blog;

public class Main {

    public static void main(String[] args) {

        //1.1 CREATING NEW ADMIN AND USER ACCOUNT
        Blog blog = new Blog();
        String username = "Tom_Hanks";
        String password = "password";
        boolean admin = true;
        blog.createNewAccount(username, password, admin);
        blog.createNewAccount("Nicolas_Cage", "password", false);

        // 1.2 LOGGING INTO THE ADMIN ACCOUNT
        AdminAccount account = (AdminAccount) blog.login(username, password);

        // 1.3 CREATING NEW TOPICS
        /*
        account.createNewTopic("AngularJS", "Typical issues encountered using AngularJS, also known as Angular1");
        account.createNewTopic("React", "Unlocking hidden power of javascript.");
        account.createNewTopic("FrontEnd development", "Various frameworks for frontend development.");

        blog.displayTopics();

        // 1.4 WRITING NEW POST
        account.writeNewPost("Why Angular JS", "HTML is great for declaring static documents, but it falters when we try to use it for declaring dynamic views in web-applications. AngularJS lets you extend HTML vocabulary for your application. The resulting environment is extraordinarily expressive, readable, and quick to develop.\n");
        account.writeNewPost("Stairway to React", "React makes it painless to create interactive UIs. Design simple views for each state in your application, and React will efficiently update and render just the right components when your data changes.\n");

        // 1.5 REGISTRING POSTS TO TOPICS
        Post angularPost = blog.findPost("Why Angular JS");
        Post reactPost = blog.findPost("Stairway to React");
        Topic angularTopic = blog.findTopic("AngularJS");
        Topic reactTopic = blog.findTopic("React");
        Topic frontEndTopic = blog.findTopic("FrontEnd development");

        angularPost.registerToTopic(angularTopic);
        angularPost.registerToTopic(frontEndTopic);
        reactPost.registerToTopic(reactTopic);
        reactPost.registerToTopic(frontEndTopic);

        // 1.6 PUBLISHING POST
        angularPost.publishPost();
        reactPost.publishPost();

        // 1.8 LOGGING INTO USER ACCOUNT
        UserAccount userAccount = (UserAccount) blog.login("Nicolas_Cage", password);

        // 1.9 READING DIFFERENT SECTIONS OF BLOG
        userAccount.readBlog();
        userAccount.readBlog("React");
        userAccount.readBlog("AngularJS");
        userAccount.readBlog("FrontEnd development");
        userAccount.readBlog("BackEnd");

        // 2.0 CHANGE POST CONTENT
        blog.getEditor().edit(angularPost);

        // 2.1 BLOCKING USER ACCOUNT
        blog.createNewAccount("Larry_Page", "password", false);
        blog.blockUserAccount("Larry_Page");
        BlogAccount b = blog.login("Larry_Page", password);

        // 2.2 SUBMITTING COMMENT TO A POST
        String commentContent = "Why would anyone user Angular, when we have other powerfull tools such as CSS and HTML?";
        angularPost.createComment(commentContent, userAccount);
        commentContent = "Nicolas, you have never been a very good actor but in compare to yours IT knowledge, I can safely say, that you chose the right carrier.";
        angularPost.createComment(commentContent, account);
        blog.readBlog("AngularJS");
*/
    }
}