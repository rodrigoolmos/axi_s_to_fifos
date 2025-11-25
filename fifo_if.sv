interface fifo_if #(
    parameter DATA_WIDTH = 32);

    logic                     clk;
    logic                     rst;

    logic [DATA_WIDTH-1:0]   wdata;
    logic                    wena;
    logic                    full;
    logic                    almost_full;

    logic [DATA_WIDTH-1:0]   rdata;
    logic                    rena;
    logic                    empty;
    logic                    almost_empty;


    // FIFO write task
    task automatic write_fifo(input logic [DATA_WIDTH-1:0] data);
        begin
            wdata = data;
            wena = 1;
            @(posedge clk);
            wena = 0;
        end
    endtask

    // FIFO read task
    task automatic read_fifo(ref logic [DATA_WIDTH-1:0] data);
        begin
            rena = 1;
            @(posedge clk);
            data = rdata;
            rena = 0;
        end
    endtask

    // FIFO empty task
    task automatic empty_fifo();
        logic [DATA_WIDTH-1:0] temp;
        begin
            while (!empty) begin
                read_fifo(temp);
            end
        end
    endtask

endinterface