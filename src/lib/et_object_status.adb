------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         OBJECT STATUS FLAGS                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- DESCRIPTION:
-- 


package body et_object_status is


	procedure set_proposed (
		status	: in out type_object_status)
	is begin
		status.proposed := true;
	end set_proposed;


	procedure clear_proposed (
		status	: in out type_object_status)
	is begin
		status.proposed := false;
	end clear_proposed;



	function is_proposed (
		status : in type_object_status)
		return boolean
	is begin
		if status.proposed then
			return true;
		else
			return false;
		end if;
	end;


	
	
	procedure set_selected (
		status	: in out type_object_status)
	is begin
		status.selected := true;
	end set_selected;


	procedure clear_selected (
		status	: in out type_object_status)
	is begin
		status.selected := false;
	end clear_selected;


	function is_selected (
		status : in type_object_status)
		return boolean
	is begin
		if status.selected then
			return true;
		else
			return false;
		end if;
	end;


	

	procedure set_moving (
		status	: in out type_object_status)
	is begin
		status.moving := true;
	end set_moving;


	procedure clear_moving (
		status	: in out type_object_status)
	is begin
		status.moving := false;
	end clear_moving;


	function is_moving (
		status : in type_object_status)
		return boolean
	is begin
		if status.moving then
			return true;
		else
			return false;
		end if;
	end;


	

	procedure set_locked (
		status	: in out type_object_status)
	is begin
		status.locked := true;
	end set_locked;


	procedure clear_locked (
		status	: in out type_object_status)
	is begin
		status.locked := false;
	end clear_locked;

	
	function is_locked (
		status : in type_object_status)
		return boolean
	is begin
		if status.locked then
			return true;
		else
			return false;
		end if;
	end;





	function get_default_status 
		return type_object_status
	is 
		s : type_object_status;
	begin
		return s;
	end get_default_status;
	


	function to_operation (
		action	: in type_action;
		flag	: in type_flag)
		return type_status_operation
	is begin 
		return (action, flag);
	end;
	




	function get_action (
		operation : in type_status_operation)
		return type_action
	is begin
		return operation.action;
	end;

	

	function get_flag (
		operation : in type_status_operation)
		return type_flag
	is begin
		return operation.flag;
	end;


	

	procedure modify_status (
		status 		: in out type_object_status;
		operation	: in type_status_operation)
	is begin
		case operation.flag is
			when SELECTED =>
				case operation.action is
					when SET =>
						set_selected (status);

					when CLEAR =>
						clear_selected (status);
				end case;

			when PROPOSED =>
				case operation.action is
					when SET =>
						set_proposed (status);

					when CLEAR =>
						clear_proposed (status);
				end case;

			when MOVING =>
				case operation.action is
					when SET =>
						set_moving (status);

					when CLEAR =>
						clear_moving (status);
				end case;

			when LOCKED =>
				case operation.action is
					when SET =>
						set_locked (status);
   
					when CLEAR =>
						clear_locked (status);
				end case;
			
		end case;
	end modify_status;
	


	procedure reset_status (
		status 		: in out type_object_status)
	is begin
		status := get_default_status;
	end reset_status;


	

	
	function to_string (
		flag : in type_flag)
		return string
	is begin
		return "flag: " & type_flag'image (flag);
	end to_string;

	
	function to_string (
		operation : in type_status_operation)
		return string
	is begin
		return "operation: " & type_action'image (operation.action)
			& " " & type_flag'image (operation.flag);
	end to_string;




	function is_A_moving (
		status : in type_AB_moving_status)
		return boolean
	is begin
		return boolean (status.A);
	end;

	
	function is_B_moving (
		status : in type_AB_moving_status)
		return boolean
	is begin
		return boolean (status.B);
	end;


	

	
	procedure set_A_moving (
		status : in out type_AB_moving_status)
	is begin
		status.A := true;
	end;


	procedure set_B_moving (
		status : in out type_AB_moving_status)
	is begin
		status.B := true;
	end;


	procedure clear_A_moving (
		status : in out type_AB_moving_status)
	is begin
		status.A := false;
	end;


	procedure clear_B_moving (
		status : in out type_AB_moving_status)
	is begin
		status.B := false;
	end;


	procedure clear_AB_moving (
		status : in out type_AB_moving_status)
	is begin
		status.A := false;
		status.B := false;
	end;

	
	
end et_object_status;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
