------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NETS LABELS                                  --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.text_io;						use ada.text_io;



package body et_net_labels is

	function to_string (appearance : in type_net_label_appearance) return string is begin
		return to_lower (type_net_label_appearance'image (appearance));
	end;

	function to_appearance (appearance : in string) return type_net_label_appearance is begin
		return type_net_label_appearance'value (appearance);
	end;
	
	function to_string (direction : in type_net_label_direction) return string is begin
		return to_lower (type_net_label_direction'image (direction));
	end;

	function to_direction (direction : in string) return type_net_label_direction is begin
		return type_net_label_direction'value (direction);
	end;



	

	procedure set_proposed (
		label : in out type_net_label_base)
	is begin
		set_proposed (label.status);
	end;

	
	procedure clear_proposed (
		label : in out type_net_label_base)
	is begin
		clear_proposed (label.status);
	end;
		

	function is_proposed (
		label : in type_net_label_base)
		return boolean
	is begin
		if is_proposed (label.status) then
			return true;
		else
			return false;
		end if;
	end is_proposed;




	procedure set_selected (
		label : in out type_net_label_base)
	is begin
		set_selected (label.status);
	end;

	
	procedure clear_selected (
		label : in out type_net_label_base)
	is begin
		clear_selected (label.status);
	end;
		

	function is_selected (
		label : in type_net_label_base)
		return boolean
	is begin
		if is_selected (label.status) then
			return true;
		else
			return false;
		end if;
	end is_selected;



	procedure modify_status (
		label 		: in out type_net_label_base;
		operation	: in type_status_operation)
	is begin
		modify_status (label.status, operation);
	end;

	

	procedure reset_status (
		label : in out type_net_label_base)
	is begin
		reset_status (label.status);
	end;




	
	

	function get_position (
		label : in type_net_label_simple)
		return type_vector_model
	is begin
		return label.position;
	end;


	function get_position (
		label : in type_net_label_simple)
		return string
	is begin
		return to_string (label.position);
	end;

	
	




	procedure set_moving (
		label : in out type_net_label_simple)
	is begin
		set_moving (label.status);
	end;

	
	procedure clear_moving (
		label : in out type_net_label_simple)
	is begin
		clear_moving (label.status);
	end;
		

	function is_moving (
		label : in type_net_label_simple)
		return boolean
	is begin
		if is_moving (label.status) then
			return true;
		else
			return false;
		end if;
	end is_moving;



	

	function get_position (
		label : in pac_net_labels.cursor)
		return type_vector_model
	is begin
		return get_position (element (label));
	end;


	function get_position (
		label : in pac_net_labels.cursor)
		return string
	is begin
		return to_string (get_position (element (label)));
	end;


	

	function is_proposed (
		label : in pac_net_labels.cursor)
		return boolean
	is begin
		return is_proposed (element (label));
	end;


	function is_selected (
		label : in pac_net_labels.cursor)
		return boolean
	is begin
		return is_selected (element (label));
	end;


	function is_moving (
		label : in pac_net_labels.cursor)
		return boolean
	is begin
		return is_moving (element (label));
	end;

	



	procedure modify_status (
		label 		: in out type_net_label_tag;
		operation	: in type_status_operation)
	is begin
		if label.active then
			modify_status (label.status, operation);
		end if;
	end;

	

	procedure reset_status (
		labels : in out type_tag_labels)
	is begin
		if labels.A.active then
			reset_status (labels.A.status);
		end if;
		
		if labels.B.active then
			reset_status (labels.B.status);
		end if;
	end;
	

	

	function is_active (
		label : in type_net_label_tag)
		return boolean
	is begin
		return label.active;
	end;
	

	procedure set_active (
		label : in out type_net_label_tag)
	is begin
		label := (active => true, others => <>);
	end;



	function is_proposed (
		label : in type_net_label_tag)
		return boolean
	is begin
		if label.active then
			return is_proposed (label.status);
		else
			return false;
		end if;
	end;

	

	procedure set_proposed (
		label : in out type_net_label_tag)
	is begin
		if label.active then
			set_proposed (label.status);
		end if;
	end;


	procedure clear_proposed (
		label : in out type_net_label_tag)
	is begin
		if label.active then
			clear_proposed (label.status);
		end if;
	end;

	



	function is_selected (
		label : in type_net_label_tag)
		return boolean
	is begin
		if label.active then
			return is_selected (label.status);
		else
			return false;
		end if;
	end;

	

	procedure set_selected (
		label : in out type_net_label_tag)
	is begin
		if label.active then
			set_selected (label.status);
		end if;
	end;


	procedure clear_selected (
		label : in out type_net_label_tag)
	is begin
		if label.active then
			clear_selected (label.status);
		end if;
	end;




	function is_moving (
		label : in type_net_label_tag)
		return boolean
	is begin
		if label.active then
			return is_moving (label.status);
		else
			return false;
		end if;
	end;
	

	procedure set_moving (
		label : in out type_net_label_tag)
	is begin
		if label.active then
			set_moving (label.status);
		end if;
	end;


	procedure clear_moving (
		label : in out type_net_label_tag)
	is begin
		if label.active then
			clear_moving (label.status);
		end if;
	end;
	
	
	
	procedure reset_tag_label (
		label : in out type_net_label_tag)
	is begin
		label := (active => false, others => <>);
	end;


	
	
end et_net_labels;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
