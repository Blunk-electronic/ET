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





-- with ada.text_io;			use ada.text_io;
package body et_net_labels is


	procedure set_proposed (
		label : in out type_net_label_base)
	is begin
		set_proposed (label.status);
	end set_proposed;


	procedure clear_proposed (
		label : in out type_net_label_base)
	is begin
		clear_proposed (label.status);
	end clear_proposed;


	function is_proposed (
		label : in type_net_label_base)
		return boolean
	is (boolean (is_proposed (label.status)));




	procedure set_selected (
		label : in out type_net_label_base)
	is begin
		set_selected (label.status);
	end set_selected;


	procedure clear_selected (
		label : in out type_net_label_base)
	is begin
		clear_selected (label.status);
	end clear_selected;


	function is_selected (
		label : in type_net_label_base)
		return boolean
	is (boolean (is_selected (label.status)));



	procedure modify_status (
		label		: in out type_net_label_base;
		operation	: in type_status_operation)
	is begin
		modify_status (label.status, operation);
	end modify_status;



	procedure reset_status (
		label : in out type_net_label_base)
	is begin
		reset_status (label.status);
	end reset_status;




	function get_rotation (
		label : in type_net_label)
		return type_rotation_documentation
	is (label.rotation);


	function get_rotation (
		label : in type_net_label)
		return string
	is (to_string (label.rotation));



	procedure set_rotation (
		label		: in out type_net_label;
		rotation	: in type_rotation_documentation)
	is begin
		label.rotation := rotation;
	end set_rotation;





	function get_position (
		label : in type_net_label)
		return type_vector_model
	is (label.position);



	procedure set_position (
		label		: in out type_net_label;
		position	: in type_vector_model)
	is begin
		label.position := position;
	end set_position;




	function get_position (
		label : in type_net_label)
		return string
	is (to_string (label.position));




	procedure move_by (
		label	: in out type_net_label;
		offset	: in type_vector_model)
	is begin
		move_by (label.position, offset);
	end move_by;





	procedure set_moving (
		label : in out type_net_label)
	is begin
		set_moving (label.status);
	end set_moving;


	procedure clear_moving (
		label : in out type_net_label)
	is begin
		clear_moving (label.status);
	end clear_moving;


	function is_moving (
		label : in type_net_label)
		return boolean
	is (boolean (is_moving (label.status)));





	function get_position (
		label : in pac_net_labels.cursor)
		return type_vector_model
	is (get_position (element (label)));


	function get_position (
		label : in pac_net_labels.cursor)
		return string
	is (to_string (get_position (element (label))));


	function get_rotation (
		label : in pac_net_labels.cursor)
		return type_rotation_documentation
	is (get_rotation (element (label)));


	function get_rotation (
		label : in pac_net_labels.cursor)
		return string
	is (get_rotation (element (label)));




	function is_proposed (
		label : in pac_net_labels.cursor)
		return boolean
	is (is_proposed (element (label)));


	function is_selected (
		label : in pac_net_labels.cursor)
		return boolean
	is (is_selected (element (label)));


	function is_moving (
		label : in pac_net_labels.cursor)
		return boolean
	is (is_moving (element (label)));



	procedure merge_labels (
		primary		: in out pac_net_labels.list;
		secondary	: in out pac_net_labels.list)
	is
		pos : pac_net_labels.cursor;
	begin
		-- Add the secondary list at the end of the primary list:
		primary.splice (source => secondary, before => pos);
	end merge_labels;




	procedure move_labels_by (
		labels	: in out pac_net_labels.list;
		offset	: in type_vector_model)
	is
		cursor : pac_net_labels.cursor := labels.first;

		-- Moves a net label candidate by the given
		-- offset:
		procedure query_label (
			label : in out type_net_label)
		is begin
			move_by (label, offset);
		end query_label;


	begin
		-- Iterate through the given list of net labels:
		while has_element (cursor) loop
			labels.update_element (cursor, query_label'access);
			next (cursor);
		end loop;
	end move_labels_by;


end et_net_labels;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
