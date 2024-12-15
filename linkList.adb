with 
  Ada.Text_IO,
  Ada.Integer_Text_IO;
use Ada;

procedure Ada_Link_List is

    type Node;
    type Node_Ptr is access Node;
    type Node is record
      Id: Integer;
      Value: Integer;
      Next: Node_Ptr;
    end record;

    procedure Print_List(Node: in Node_Ptr) is
    begin
      if Node /= null then
        Integer_Text_IO.Put(Node.Id);
        Integer_Text_IO.Put(Node.Value);
        Ada.Text_IO.New_Line;
        Print_List(Node.Next);
      end if;
    end Print_List;

    procedure Append_Node(Id: Integer; Value: Integer; New_Node: in out Node_Ptr) is
    begin
      if New_Node = null then
        New_Node := new Node'(Id,Value,null);
        Ada.Text_IO.Put("Adding Head node");
        Ada.Text_IO.New_Line;
      else
        if New_Node.Next /= null then
          Append_Node(Id, Value, New_Node.Next);
        else
          Ada.Text_IO.Put("Adding Next node");
          Ada.Text_IO.New_Line;
          New_Node.Next := new Node'(Id, Value, null);
        end if;
      end if;
    end Append_Node;

    Head : Node_Ptr := null;

begin
    Append_Node(1, 1, Head);
    Append_Node(2, 2, Head);
    Append_Node(3, 3, Head);
    Append_Node(4, 4, Head);
    Append_Node(5, 5, Head);
    Print_List(Head);
    Append_Node(6, 6, Head);
    Print_List(Head);
end Ada_Link_List;
