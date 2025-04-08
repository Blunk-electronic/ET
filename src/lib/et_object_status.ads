------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         OBJECT STATUS FLAGS                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
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


package et_object_status is

	type type_proposed is new boolean;

	type type_selected is new boolean;

	type type_moving is new boolean;

	type type_locked is new boolean;


	type type_object_status is private;
	
	

	procedure set_proposed (
		status	: in out type_object_status);


	procedure clear_proposed (
		status	: in out type_object_status);


	function is_proposed (
		status : in type_object_status)
		return boolean;
	

	
	procedure set_selected (
		status	: in out type_object_status);


	procedure clear_selected (
		status	: in out type_object_status);


	function is_selected (
		status : in type_object_status)
		return boolean;


	

	procedure set_moving (
		status	: in out type_object_status);


	procedure clear_moving (
		status	: in out type_object_status);


	function is_moving (
		status : in type_object_status)
		return boolean;


	

	procedure set_locked (
		status	: in out type_object_status);


	procedure clear_locked (
		status	: in out type_object_status);


	function is_locked (
		status : in type_object_status)
		return boolean;

		

	procedure reset (
		status : in out type_object_status);


	function get_default_status 
		return type_object_status;
	
	
	type type_action is (SET, CLEAR);

	type type_flag is (PROPOSED, SELECTED, MOVING, LOCKED);

	type type_status_operation is record
		action	: type_action;
		flag	: type_flag;
	end record;



	procedure modify_status (
		status 		: in out type_object_status;
		operation	: in type_status_operation);

	
	procedure reset_status (
		status 		: in out type_object_status);

	
	
	function to_string (
		flag : in type_flag)
		return string;

	
	function to_string (
		operation : in type_status_operation)
		return string;

	
private
	type type_object_status is record
		proposed	: type_proposed := false;
		selected	: type_selected := false;
		moving		: type_moving := false;
		locked		: type_locked := false;
	end record;


	
end et_object_status;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
