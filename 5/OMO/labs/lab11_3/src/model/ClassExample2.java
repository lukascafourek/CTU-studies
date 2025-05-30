package model;

import securityAndValidation.Email;
import securityAndValidation.NotNull;
import securityAndValidation.UiUserRoles;

//@NotNull(message="all attributes can not be null.")
public class ClassExample2{
	//@NotNull(message="attr1 can not be null.")
	@UiUserRoles(role={"ROLE_ADMIN","ROLE_USER"})
	String attr01;
	
	public ClassExample2(){
		this.attr01 = "";
	}
	
}
