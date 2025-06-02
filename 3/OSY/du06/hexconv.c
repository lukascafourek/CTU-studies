int read(int fd, void *buf, int length) {
    int ret;
    asm volatile (
        "int $0x80"
        : "=a" (ret)
        : "a"(3), "b"(fd), "c"(buf), "d"(length)
        : "memory");
    return ret;
}

int write(int fd, const void *buf, int length) {
    int ret;
    asm volatile (
        "int $0x80"
        : "=a" (ret)
        : "a"(4), "b"(fd), "c"(buf), "d"(length)
        : "memory");
    return ret;
}

void exit(int status) {
    asm volatile (
        "int $0x80"
        :
        : "a"(1), "b"(status)
    );
}

void hexconv(unsigned num, char *buf) {
    static const char hex_digits[] = "0123456789abcdef";
    for (int i = 7; i >= 0; i--) {
        buf[i] = hex_digits[num % 16];
        num /= 16;
    }
    buf[8] = '\n';
    buf[9] = '\0';
    int idx = 0, i = 0, first_nonzero = 0;
    while (first_nonzero == 0) {
        if (buf[i] == '0')
            i++;
        else
            first_nonzero = 1;
    }
    if (i == 8) {
        buf[0] = '0';
        buf[1] = 'x';
        buf[2] = '0';
        buf[3] = '\n';
        for (int j = 4; j < 20; j++)
            buf[j] = '\0';
        return;
    }
    while (i < 8)
        buf[idx++] = buf[i++];
    for (int j = idx - 1; j >= 0; j--)
        buf[j + 2] = buf[j];
    buf[0] = '0';
    buf[1] = 'x';
    buf[idx + 2] = '\n';
    for (int j = idx + 3; j < 20; j++)
        buf[j] = '\0';
}

static void print(unsigned num) {
    char buf[20];
    hexconv(num, buf);
    int ret = write(1, buf, sizeof(buf));
    if (ret == -1)
        exit(1);
}

int isnum(char ch) {
    return ch >= '0' && ch <= '9';
}

int isspc(char ch) {
    return ch == ' ' || ch == '\n';
}

void _start(void) {
    char buf[20];
    unsigned num = 0, chars_to_process = 0;
    int i, num_digits = 0;
    while (1) {
        if (chars_to_process == 0) {
            int ret = read(0, buf, sizeof(buf));
            if (ret < 0)
                exit(1);
            i = 0;
            chars_to_process = ret;
        }
        if (num_digits > 0 && (chars_to_process == 0 || !isnum(buf[i]))) {
            print(num);
            num_digits = 0;
            num = 0;
        }
        if (chars_to_process == 0 || (!isspc(buf[i]) && !isnum(buf[i])))
            exit(0);
        if (isnum(buf[i])) {
            num = num * 10 + buf[i] - '0';
            num_digits++;
        }
        i++;
        chars_to_process--;
    }
}
