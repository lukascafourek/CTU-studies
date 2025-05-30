package model;

import java.util.ArrayList;

import securityAndValidation.Email;
import securityAndValidation.Max;
import securityAndValidation.Min;
import securityAndValidation.NotNull;
import securityAndValidation.Password;
import securityAndValidation.UiUserRoles;

//@Email(message="all atributes must be in email format.")
//@Password
@UiUserRoles(role={"ROLE_USER"})
public class ClassExample{
	@NotNull(message="attr1 can not be null.")
	@Password
	String attr1;
	@UiUserRoles(role={"ROLE_ADMIN","ROLE_USER"})
	@NotNull(message="attr2 can not be null.")
	int attr2;
	@UiUserRoles(role={"ROLE_USER"})
	@Email(message="attr3 is not in email format.")
	String attr3;
	
	@Min(size=1, message="the length of attr4 must be bigger than 1")
	@Max(size=5, message="the length of attr4 must be smaller than 5")
	String attr4;
	
	public ClassExample(){
		this.attr1 = "";//delete value to test annotation
		this.attr2 = 1;
		this.attr3 = "example";
		this.attr4 = "aaaaaaaa";
	}
	
}
